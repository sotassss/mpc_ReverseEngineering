import re
from uuid import uuid4
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.documents import Document
import tiktoken

from src.model_types import ScriptAnalysisResult, ScriptAnalysisResults

from src.utils.config import load_config
from src.utils.file_utiles import is_sensitive_file, extract_text

MAX_TOKENS=120_000

encoding=tiktoken.encoding_for_model("gpt-4o-mini")

class ScriptAnalysisNode:
    def __init__(self, db, llm):
        self.db = db
        self.llm = llm.with_structured_output(ScriptAnalysisResult)
        self.config = load_config()
        
    def run(self, source_files: list[str]) -> ScriptAnalysisResults:
        """
        ソースコードのパスのリストを受け取り、すべての要約を作成するノード
        Args:
            source_files: ソースコードのパスのリスト
        Returns:
            分析結果のリスト
        """
        texts = []

        # LLMにパスワード情報などをアップロードしないようにするための処理
        for source in source_files:
            if is_sensitive_file(source, self.config):
                texts.append({"text": "センシティブなファイルのため内容は非表示です。", "path": source})
                continue
                
            text = extract_text(source)
            # テキストのトークン数を取得
            if text:
                text_tokens=encoding.encode(text,allowed_special={"<|endoftext|>"})
            else:
                text_tokens=[]

            # None の場合または空文字列の場合はスキップ
            if text is None or not text:
                continue
                
            # センシティブな内容のフィルタリング
            is_sensitive_content = False
            for pattern in self.config.get("sensitive_content_patterns", []):
                if re.search(pattern, text, re.IGNORECASE):
                    texts.append({"text": "センシティブなファイルのため内容は非表示です。", "path": source})
                    is_sensitive_content = True
                    break
                    
            if is_sensitive_content:
                continue
                
            # トークン数制限に達するファイルをスキップ
            if len(text_tokens) > MAX_TOKENS:
                print(f"⚠ ファイルが長すぎるためスキップ: {source} (トークン数：{len(text_tokens)})")
                continue

            texts.append({"text": text, "path": source})

        prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "あなたはコード解析のスペシャリストです。"
                ),
                (
                    "human",
                    "以下のソースコードの簡単な概要と詳細な説明を作成してください。\n\n"
                    "詳細な説明には各関数や変数の詳細な役割や定義などを含め、全体の処理内容も分かりやすいようにまとめてください。\n"
                    "簡単な概要については、簡潔にそのソースコードの役割を説明するようにしてください。\n\n"
                    "「センシティブなファイルのため内容は非表示です。」と書かれているものは、推測をせずに、非表示と記述してください。\n\n"
                    "一般的な内容を推測して記述してはいけません。\n\n"
                    "ファイルパス: {path}\n\n"
                    "# ソースコード\n"
                    "{text}"
                )
            ]
        )

        # chainの定義
        chain = prompt | self.llm

        ##################################手法1: バッチ処理を使用して並列処理##################################
        results = list(chain.batch([text for text in texts]))   # ジェネレータを直接リストに変換
        
        # ファイルパスと結果を対応付ける
        file_paths = [item["path"] for item in texts]
        
        # ドキュメントをベクトルストアに追加
        docs = [
            Document(page_content=result.detail_explanation, metadata={"file_path": file_path})
            for file_path, result in zip(file_paths, results)
        ]

        uuids = [str(uuid4()) for _ in range(len(docs))]
        self.db.add_documents(documents=docs, ids=uuids)

        return results
    

        ##################################手法2:トークン制限回避のため直列処理##################################
        # results = []
        # docs = []
        # uuids = []
        
        # # 各ファイルを個別に処理
        # for idx,text_item in enumerate(texts,start=1):
        #     try:
        #         # 個別にLLMを呼び出し
        #         result = chain.invoke(text_item)
        #         results.append(result)
                
        #         # ドキュメントを作成
        #         doc = Document(
        #             page_content=result.detail_explanation, 
        #             metadata={"file_path": text_item["path"]}
        #         )
        #         docs.append(doc)
        #         uuids.append(str(uuid4()))
                
        #         print(f"✓ 処理完了({idx}/{len(texts)}): {text_item['path']}")
                
        #     except Exception as e:
        #         print(f"※ ファイル処理中にエラー発生({idx}/{len(texts)}): {text_item['path']}")
        #         print(f"エラー内容: {str(e)}")
                
        #         # エラーが発生した場合はエラー情報を含む結果を作成
        #         fallback_result = ScriptAnalysisResult(
        #             summary=f"処理エラー: {text_item['path']}",
        #             detail_explanation=f"このファイルの処理中にエラーが発生しました: {str(e)}",
        #             file_path=text_item["path"],  
        #             short_summary=f"処理エラー"
        #         )
        #         results.append(fallback_result)
        
        # # 処理したドキュメントがあればベクトルストアに追加
        # if docs:
        #     self.db.add_documents(documents=docs, ids=uuids)
        
        # return results
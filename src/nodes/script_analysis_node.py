import re
from uuid import uuid4
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.documents import Document
import tiktoken

from src.model_types import ScriptAnalysisResult, ScriptAnalysisResults

from src.utils.config import load_config
from src.utils.file_utiles import is_sensitive_file, extract_text, check_extension
from src.utils.proxy_on_off import handle_proxy_request

from dotenv import load_dotenv

load_dotenv()
 
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
        for idx, source in enumerate(source_files):
            # 拡張子の確認
            if check_extension(source, self.config):
                print(f"{idx + 1}/{len(source_files)} ※ スキップ  ファイル：{source} (拡張子により対象外)")
                continue
            
            # センシティブなファイルかの確認
            if is_sensitive_file(source, self.config):
                texts.append({"text": "センシティブなファイルのため内容は非表示です。", "path": source})
                print(f"{idx + 1}/{len(source_files)} ※ スキップ  ファイル：{source} (センシティブな内容が含まれるファイル)")
                continue

            chunks = extract_text(source)

            # None の場合または空リストの場合はスキップ
            if not chunks:
                print(f"{idx + 1}/{len(source_files)} ※ エラー    ファイル：{source} (エンコーディングを検出できないファイル)")
                continue

            # 各チャンクに対してセンシティブチェック
            is_sensitive_content = False
            for chunk in chunks:
                for pattern in self.config.get("sensitive_content_patterns", []):
                    if re.search(pattern, chunk, re.IGNORECASE):
                        texts.append({"text": "センシティブなファイルのため内容は非表示です。", "path": source})
                        is_sensitive_content = True
                        break
                if is_sensitive_content:
                    break

            if is_sensitive_content:
                print(f"{idx + 1}/{len(source_files)} ※ スキップ  ファイル：{source} (センシティブな内容が含まれるファイル)")
                continue

            print(f"{idx + 1}/{len(source_files)} ✔ 処理完了  ファイル: {source}（分割数: {len(chunks)})")

            # チャンクごとに追加（順序保持）
            for chunk in chunks:
                texts.append({"text": chunk, "path": source})

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
                    "ファイルパス: {path}\n\n"
                    "# ソースコード\n"
                    "{text}"
                )
            ]
        )

        # chainの定義
        chain = prompt | self.llm

        ##################################手法1: バッチ処理を使用して並列処理##################################
        # バッチサイズを設定してバッチ処理を実行
        # BATCH_SIZE = 50  # 一度に処理するファイル数を調整

        # results = []
        # # テキストをバッチに分割して処理
        # for i in range(0, len(texts), BATCH_SIZE):
        #     handle_proxy_request()  # プロキシ設定を自動で切り替える処理
        #     batch_texts = texts[i:i + BATCH_SIZE]
        #     print(f"バッチ処理中: {i+1}〜{min(i+BATCH_SIZE, len(texts))}/{len(texts)}")
            
        #     # バッチ処理の実行
        #     batch_results = list(chain.batch([text for text in batch_texts]))
        #     results.extend(batch_results)

        # # ファイルパスと結果を対応付ける
        # file_paths = [item["path"] for item in texts]

        # # ドキュメントをベクトルストアに追加
        # docs = [
        #     Document(page_content=result.detail_explanation, metadata={"file_path": file_path})
        #     for file_path, result in zip(file_paths, results)
        # ]

        # uuids = [str(uuid4()) for _ in range(len(docs))]
        # self.db.add_documents(documents=docs, ids=uuids)

        # return results
    

        ##################################手法2:トークン数を計算してバッチ処理##################################
        MAX_PROMPT_TOKENS = 100000  # GPT-4.1-nanoの場合
        results = []
        batch = []
        batch_tokens = 0

        def count_tokens(text):
            return len(encoding.encode(text["text"]))

        file_paths = []

        # 全体の進捗表示
        total_texts = len(texts)

        for idx, text in enumerate(texts):
            text_tokens = count_tokens(text)

            # トークン上限を超える場合は一度送信
            if batch_tokens + text_tokens > MAX_PROMPT_TOKENS:
                if batch:
                    try:
                        handle_proxy_request()
                        print(f"バッチ処理中: {len(batch)}件 (合計トークン数: {batch_tokens})")
                        batch_results = list(chain.batch(batch))
                        results.extend(batch_results)
                        file_paths.extend([item["path"] for item in batch])
                    except Exception as e:
                        print(f"※ バッチ送信中にエラー: {e}")
                    batch = []
                    batch_tokens = 0

            batch.append(text)
            batch_tokens += text_tokens

            # 進捗表示
            progress = (idx + 1) / total_texts * 100
            print(f"バッチ処理進捗: {idx + 1}/{total_texts}件 完了 ({progress:.2f}%)")

        # 最後のバッチを処理
        if batch:
            try:
                handle_proxy_request()
                print(f"バッチ処理中: {len(batch)}件 (合計トークン数: {batch_tokens})")
                batch_results = list(chain.batch(batch))
                results.extend(batch_results)
                file_paths.extend([item["path"] for item in batch])
            except Exception as e:
                print(f"※ バッチ送信中にエラー: {e}")

        # ベクトルストアに保存
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
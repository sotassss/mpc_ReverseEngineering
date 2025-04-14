import re
from uuid import uuid4
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.documents import Document

from src.model_types import ScriptAnalysisResult, ScriptAnalysisResults

from src.utils.config import load_config
from src.utils.file_utiles import is_sensitive_file, extract_text

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
                text = "センシティブなファイルのため内容は非表示です。" # センシティブな内容を含む場合は内容を表示しない
            else:
                text = extract_text(source) # ファイルからテキストを抽出
                if text:
                    for pattern in self.config.get("sensitive_content_patterns", []):
                        if re.search(pattern, text, re.IGNORECASE):
                            text = "センシティブなファイルのため内容は非表示です。"
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
                    "ファイルパス: {path}\n\n"
                    "# ソースコード\n"
                    "{text}"
                )
            ]
        )

        # chainの定義
        chain = prompt | self.llm

        # リストをバッチで並列処理
        results = chain.batch_as_completed(texts)

        # ドキュメントをベクトルストアに追加
        docs = [
            Document(page_content=result.detail_explanation, metadata={"file_path": file_path})
            for file_path, result in results
        ]

        uuids = [str(uuid4()) for _ in range(len(docs))]

        self.db.add_documents(documents=docs, ids=uuids)

        return results
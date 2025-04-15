from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import StrOutputParser
from operator import itemgetter

from src.model_types import Sections, GeneratedDocument

class DocumentGenerationNode:
    def __init__(self, llm, db, k=10):
        self.llm = llm
        self.db = db
        self.retriever = db.as_retriever(k=k)

    def run(self, sections: Sections) -> GeneratedDocument:
        """
        各セクションのドキュメントを生成するノード、生成の際にはベクトルストアから取得したスクリプト情報を参照する
        Args:
            sections: 生成されたセクション構成
        Returns:
            生成されたドキュメント
        """
        prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "あなたはシステムの仕様書を作成するライターです。"
                ),
                (
                    "human",
                    "あなたは以下に指定される仕様書のセクションのライティングを担当します。\n"
                    "なるべく詳細なドキュメントを作成するようにしてください。ただし、ソースコードの情報から判断できない主張は行わないでください。\n"
                    "仕様書はマークダウン形式で記述し、文頭には必ず見出しとしてセクション名を含めてください。\n"
                    "内容の指示に従って、ソースコードの内容をもとに詳細で包括的な仕様書を作成してください。\n\n"
                    "セクション名: {section_name}\n"
                    "内容の指示: {section_description}\n\n"
                    "各ソースコードの内容: \n"
                    "{context}"
                )
            ]
        )
        
        # 各セクションに対して行う処理のchainを定義
        chain = {
            "section_name": itemgetter("section_name"),
            "section_description": itemgetter("section_description"),
            "context": itemgetter("section_description") | self.retriever | self._format_contents
        } | prompt | self.llm | StrOutputParser()

        # セクションごとに並列処理(出力がリストになるように注意)
        results = list(chain.batch_as_completed([
            {
                "section_name": s.section_name, 
                "section_description": s.section_description
            } for s in sections.sections
        ]))
        documents = [doc for _, doc in results]

        return GeneratedDocument(
            title=sections.title, 
            documents=documents
        )

    
    def _format_contents(self, docs):
        """
        retrieverから取得したcontextを整形する関数
        """
        contents = "\n\n".join([f"パス: {doc.metadata['file_path']}\n内容:\n{doc.page_content}" for doc in docs])
        return contents

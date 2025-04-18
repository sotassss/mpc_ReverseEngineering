from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import StrOutputParser
from operator import itemgetter
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import List

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
                    "仕様書はマークダウン形式で記述し、文頭には必ず見出しとしてセクション名を含めてください。 \n"
                    "「センシティブなファイルのため内容は非表示です。」と書かれているものは、推測をせずに、非表示としてください。\n\n"
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

        # ThreadPoolExecutorを使用して並列処理しつつ順序を保持する
        documents = self._process_sections_parallel(chain, sections.sections)
        
        return GeneratedDocument(
            title=sections.title,
            documents=documents
        )
    
    #  並列処理の実装
    def _process_sections_parallel(self, chain, sections_list):
        """
        ThreadPoolExecutorを使用してセクションを並列処理しながら順番を保持する
        """
        # セクションごとの処理を関数化
        def process_section(index_and_section):
            index, section = index_and_section
            input_data = {
                "section_name": section.section_name,
                "section_description": section.section_description
            }
            output = chain.invoke(input_data)
            return index, output
        
        # 結果を格納するリスト（インデックス付き）
        results = [None] * len(sections_list)
        
        # ThreadPoolExecutorで並列処理
        with ThreadPoolExecutor() as executor:
            # タスクをサブミット（インデックス付き）
            future_to_index = {
                executor.submit(process_section, (i, section)): i
                for i, section in enumerate(sections_list)
            }
            
            # 完了したタスクから結果を取得
            for future in as_completed(future_to_index):
                index, result = future.result()
                results[index] = result
        
        # リストには既に順序通りに結果が格納されている
        return results
        
    def _format_contents(self, docs):
        """
        retrieverから取得したcontextを整形する関数
        """
        contents = "\n\n".join([f"パス: {doc.metadata['file_path']}\n内容:\n{doc.page_content}" for doc in docs])
        return contents
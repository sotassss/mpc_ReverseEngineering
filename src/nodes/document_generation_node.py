from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import StrOutputParser
from operator import itemgetter
from concurrent.futures import ThreadPoolExecutor, as_completed
from langchain_chroma import Chroma
from langchain_core.runnables import RunnableLambda, RunnablePassthrough

from src.model_types import Sections, GeneratedDocument
from src.model_types import ScriptAnalysisResults, Sections

class DocumentGenerationNode:
    def __init__(self, llm, db, k=10):
        self.llm = llm
        self.db = db
        self.retriever = db.as_retriever(k=k)
        self.summaries = None  # 初期化時はNone、後で設定する
    
    def set_summaries(self, summaries: ScriptAnalysisResults):
        """
        スクリプト解析結果を設定する
        Args:
            summaries: スクリプト解析結果
        """
        self.summaries = summaries
    
    def run(self, sections: Sections) -> GeneratedDocument:
        """
        各セクションのドキュメントを生成するノード、生成の際にはベクトルストアから取得したスクリプト情報を参照する
        Args:
            sections: 生成されたセクション構成
        Returns:
            生成されたドキュメント
        """

        # ルーターチェーンを作成
        router_chain = self._create_router_chain()
        
        # RAG チェーンを作成
        rag_chain = self._create_rag_chain()
        
        # LCチェーンを作成
        lc_chain = self._create_lc_chain(self.summaries)
        
        # セクションをルーティングして処理する関数
        def route_and_process_section(section_data):
            """
            セクションをルーティングして適切なチェーンで処理する
            Args:
                section_data: セクション情報
            Returns:
                生成されたドキュメント
            """
            # ルーターでセクションを評価
            route_result = router_chain.invoke({
                "section_name": section_data["section_name"],
                "section_description": section_data["section_description"]
            })
            
            # ルーターの結果に基づいてチェーンを選択
            if "全体を見る必要あり" in route_result:
                # 全体を見る必要がある場合は LC チェーンを使用
                print(f"{section_data["section_name"]} : LC")
                return lc_chain.invoke(section_data)
            else:
                # それ以外の場合は RAG チェーンを使用
                print(f"{section_data["section_name"]} : RAG")
                return rag_chain.invoke(section_data)
        
        # ThreadPoolExecutorを使用して並列処理しつつ順序を保持する
        documents = self._process_sections_with_routing(route_and_process_section, sections.sections)
        
        return GeneratedDocument(
            title=sections.title,
            documents=documents
        )
    
    # ルーターのチェーン
    def _create_router_chain(self):
        """
        セクションごとに適切なチェーンを決定するルーターチェーンを作成
        
        Returns:
            ルーターチェーン
        """
        prompt = ChatPromptTemplate.from_messages([
            (
                "system",
                "あなたは与えられたセクションの内容に基づいて、全体を見る必要があるかどうかを判断する専門家です。"
            ),
            (
                "human",
                "以下のセクション名と内容の指示に基づいて、このセクションを生成するために全体を見る必要があるかどうかを判断してください。\n\n"
                "セクション名: {section_name}\n"
                "内容の指示: {section_description}\n\n"
                "もし全体像を把握する必要があるセクション（例：「概要」「アーキテクチャ」「設計方針」など）であれば、「全体を見る必要あり」と回答してください。\n"
                "もし特定のコンポーネントやモジュールに焦点を当てたセクションであれば、「部分的な情報で十分」と回答してください。\n\n"
                "回答は「全体を見る必要あり」または「部分的な情報で十分」のいずれかのみを返してください。"
            )
        ])
        
        chain = prompt | self.llm | StrOutputParser()
        return chain
    
    # RAGを利用したチェーン
    def _create_rag_chain(self):
        """
            RAG(Retrieval Augmented Generation)のチェーンを作成
            全体を見る必要がないセクションの執筆
        """
        prompt = ChatPromptTemplate.from_messages([
            (
                "system",
                "あなたはシステムの仕様書を作成するライターです。"
            ),
            (
                "human",
                "あなたは以下に指定される仕様書のセクションのライティングを担当します。\n"
                "なるべく詳細なドキュメントを作成するようにしてください。ただし、ソースコードの情報から判断できない主張は行わないでください。\n"
                "仕様書はマークダウン形式で記述し、文頭には必ず見出しとしてセクション名を含めてください。 \n"
                "「センシティブなファイルのため内容は非表示です。」と書かれているものは、推測をせずに非表示としてください。\n\n"
                "一般的な内容を推測して記述してはいけません。\n\n"
                "センシティブなファイルの内容は記述しないでください。説明も不要です。\n\n"
                "内容の指示に従って、ソースコードの内容をもとに詳細で包括的な仕様書を作成してください。\n\n"
                "セクション名: {section_name}\n"
                "内容の指示: {section_description}\n\n"
                "各ソースコードの内容: \n"
                "{context}"
            )
        ])

        chain = {
            "section_name": itemgetter("section_name"),
            "section_description": itemgetter("section_description"),
            "context": itemgetter("section_description") | self.retriever | self._format_contents
        } | prompt | self.llm | StrOutputParser()
        
        return chain
    
    # Summaryを利用するチェーン   
    def _create_lc_chain(self, summaries: ScriptAnalysisResults):
        """
        全体を見る必要があるセクションの執筆・Summariesを元に作成
        
        Args:
            summaries: スクリプト解析結果
        """
        prompt = ChatPromptTemplate.from_messages([
            (
                "system",
                "あなたはシステムの仕様書を作成するライターです。"
            ),
            (
                "human",
                "あなたは以下に指定される仕様書のセクションのライティングを担当します。\n"
                "なるべく詳細なドキュメントを作成するようにしてください。ただし、ソースコードの情報から判断できない主張は行わないでください。\n"
                "仕様書はマークダウン形式で記述し、文頭には必ず見出しとしてセクション名を含めてください。 \n"
                "要約の書類を丁寧に網羅するようにしてください。\n\n" 
                "センシティブなファイルの内容は記述しないでください。説明も不要です。\n\n"
                "内容の指示に従って、ソースコードの内容をもとに詳細で包括的な仕様書を作成してください。\n\n"
                "セクション名: {section_name}\n"
                "内容の指示: {section_description}\n\n"
                "各ソースコードの内容: \n"
                "{summaries_content}"
            )
        ])

        def add_summaries(inputs):
            """サマリー情報を入力に追加する"""
            return {
                "section_name": inputs["section_name"],
                "section_description": inputs["section_description"],
                "summaries_content": summaries
            }

        # チェーンの定義
        chain = RunnablePassthrough() | RunnableLambda(add_summaries) | prompt | self.llm | StrOutputParser()
        
        return chain
    
    def _process_sections_with_routing(self, route_and_process_func, sections_list):
        """
        ThreadPoolExecutorを使用してセクションをルーティングしながら並列処理し、順番を保持する
        
        Args:
            route_and_process_func: ルーティングと処理を行う関数
            sections_list: セクションのリスト
        Returns:
            処理結果のリスト（順序通り）
        """
        # セクションごとの処理を関数化
        def process_section(index_and_section):
            index, section = index_and_section
            input_data = {
                "section_name": section.section_name,
                "section_description": section.section_description
            }
            # 指定された関数でルーティングと処理を実行
            output = route_and_process_func(input_data)
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
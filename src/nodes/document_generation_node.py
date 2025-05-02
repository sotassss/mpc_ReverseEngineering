from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import StrOutputParser, PydanticOutputParser
from operator import itemgetter
from langchain_chroma import Chroma
from langchain_core.runnables import RunnableLambda, RunnablePassthrough

from src.model_types import Sections, GeneratedDocument, RoutingDecision
from src.model_types import ScriptAnalysisResults, Sections

class DocumentGenerationNode:
    def __init__(self, llm, db, k=10):
        self.llm = llm
        self.db = db
        self.retriever = db.as_retriever(k=k)
        self.summaries = None  # 初期化時はNone、後で設定する
        self.routing_decisions = {}  # 各セクションの判断理由を保存
    
    def set_summaries(self, summaries: ScriptAnalysisResults):
        """
        スクリプト解析結果を設定する
        Args:
            summaries: スクリプト解析結果
        """
        self.summaries = summaries
    
    def get_routing_decisions(self):
        """
        各セクションのルーティング判断結果を取得する
        Returns:
            ルーティング判断結果の辞書
        """
        return self.routing_decisions
    
    def run(self, sections: Sections) -> GeneratedDocument:
        """
        各セクションのドキュメントを生成するノード、生成の際にはベクトルストアから取得したスクリプト情報を参照する
        Args:
            sections: 生成されたセクション構成
        Returns:
            生成されたドキュメント
        """
        # 各セクションのルーティング判断を一括実行
        section_inputs = [
            {
                "section_name": section.section_name,
                "section_description": section.section_description
            }
            for section in sections.sections
        ]
        
        # 1. ルーティング判断をバッチ処理
        router_chain = self._create_router_chain()
        routing_results = router_chain.batch(section_inputs)
        
        # 2. セクションごとに適切なチェーンを選択
        rag_inputs = []
        lc_inputs = []
        
        for i, (section_input, route_result) in enumerate(zip(section_inputs, routing_results)):
            section_name = section_input["section_name"]
            
            # ルーティング判断結果と理由を保存
            self.routing_decisions[section_name] = route_result
            
            if route_result.decision == "全体を見る必要あり":
                print(f" - {section_name} : LC ")
                # print(f" - {section_name} : LC (理由: {route_result.reasoning})")
                lc_inputs.append((i, section_input))
            else:
                print(f" - {section_name} : RAG ")
                # print(f" - {section_name} : RAG (理由: {route_result.reasoning})")
                rag_inputs.append((i, section_input))
        
        # 3. RAG チェーンと LC チェーンをバッチ処理で並列実行
        rag_chain = self._create_rag_chain()
        lc_chain = self._create_lc_chain(self.summaries)
        
        # バッチ処理実行
        rag_results = self._batch_process_with_indices(rag_chain, [input_data for _, input_data in rag_inputs]) if rag_inputs else []
        lc_results = self._batch_process_with_indices(lc_chain, [input_data for _, input_data in lc_inputs]) if lc_inputs else []
        
        # 4. 結果を元の順序に戻す
        results = [None] * len(sections.sections)
        
        for (i, _), result in zip(rag_inputs, rag_results):
            results[i] = result
            
        for (i, _), result in zip(lc_inputs, lc_results):
            results[i] = result
        
        return GeneratedDocument(
            title=sections.title,
            documents=results
        )
    
    def _batch_process_with_indices(self, chain, inputs):
        """
        チェーンをバッチ処理で実行する
        
        Args:
            chain: 実行するチェーン
            inputs: 入力データのリスト
        Returns:
            結果のリスト
        """
        return list(chain.batch(inputs))
    
    # ルーターのチェーン - 判断理由を含むように改善
    def _create_router_chain(self):
        """
        セクションごとに適切なチェーンを決定するルーターチェーンを作成
        判断理由も含めた構造化データを返す
        
        Returns:
            ルーターチェーン
        """
        prompt = ChatPromptTemplate.from_messages([
            (
                "system",
                "あなたは与えられたセクションの内容に基づいて、全体を見る必要があるかどうかを判断する専門家です。"
                "判断結果と共に、その理由も説明してください。"
            ),
            (
                "human",
                "以下のセクション名と内容の指示に基づいて、このセクションを生成するために全体を見る必要があるかどうかを判断してください。\n\n"
                "セクション名: {section_name}\n"
                "内容の指示: {section_description}\n\n"
                "以下の判断基準を参考にしてください：\n"
                "- 「全体を見る必要あり」：概要、アーキテクチャ全体、設計方針、システム間の関係性、全体像を理解する必要がある場合\n"
                "- 「部分的な情報で十分」：特定のコンポーネント、モジュール、関数に焦点を当てた内容、局所的な情報のみで説明可能な場合\n\n"
                "出力は以下のJSON形式で返してください：\n"
                "```\n"
                "{{\n"
                '  "decision": "全体を見る必要あり" または "部分的な情報で十分",\n'
                '  "reasoning": "判断理由の説明"\n'
                "}}\n"
                "```"
            )
        ])
        
        # PydanticOutputParserを使用して構造化データを返す
        parser = PydanticOutputParser(pydantic_object=RoutingDecision)
        
        chain = prompt | self.llm | parser
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
                "図を記述したい場合は mermaid 形式で記述してください。 \n"
                "Mermaid のノードラベル内に全角の括弧（や）や記号を使用しないでください。\n\n"
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

        def get_context(input_data):
            """検索結果を取得して整形する関数"""
            docs = self.retriever.invoke(input_data["section_description"])
            return self._format_contents(docs)
        
        # コンテキスト取得をバッチ処理対応に変更
        retriever_chain = RunnableLambda(get_context)
        
        chain = {
            "section_name": itemgetter("section_name"),
            "section_description": itemgetter("section_description"),
            "context": RunnableLambda(lambda x: retriever_chain.invoke(x))
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
                "図を記述したい場合は mermaid 形式で記述してください。 \n"
                "要約の書類を丁寧に網羅するようにしてください。\n\n" 
                "センシティブなファイルの内容は記述しないでください。説明も不要です。\n\n"
                "内容の指示に従って、ソースコードの内容をもとに詳細で包括的な仕様書を作成してください。\n\n"
                "セクション名: {section_name}\n"
                "内容の指示: {section_description}\n\n"
                "各章の要約: \n"
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
        
    def _format_contents(self, docs):
        """
        retrieverから取得したcontextを整形する関数
        """
        contents = "\n\n".join([f"パス: {doc.metadata['file_path']}\n内容:\n{doc.page_content}" for doc in docs])
        return contents

    def generate_routing_report(self):
        """
        セクションのルーティング判断レポートを生成する
        Returns:
            ルーティングレポート文字列
        """
        if not self.routing_decisions:
            return "ルーティング判断のレポートはまだ生成されていません。"
            
        report = "# セクションルーティング判断レポート\n\n"
        report += "| セクション名 | 判断結果 | 判断理由 |\n"
        report += "|------------|---------|--------|\n"
        
        for section_name, decision in self.routing_decisions.items():
            report += f"| {section_name} | {decision.decision} | {decision.reasoning} |\n"
            
        return report
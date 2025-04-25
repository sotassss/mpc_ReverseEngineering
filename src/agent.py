from typing import Any
from langgraph.graph import END, StateGraph

from src.state import  State
from src.model_types import GeneratedDocument

from src.nodes.script_analysis_node import ScriptAnalysisNode
from src.nodes.document_orchestration_node import DocumentOrchestrationNode
from src.nodes.document_generation_node import DocumentGenerationNode
from src.nodes.consistency_check_node import ConsistencyCheckNode
from src.model_types import ScriptAnalysisResults, Sections, GeneratedDocument
from src.utils.proxy_on_off import handle_proxy_request

class ReverseEngine:
    def __init__(self, llm, db, k=10, maximum_iteration=2):
        self.maximum_iteration = maximum_iteration
        # ノードの初期化
        self.script_analysis_node = ScriptAnalysisNode(db=db, llm=llm)
        self.document_orchestration_node = DocumentOrchestrationNode(llm=llm)
        self.document_generation_node = DocumentGenerationNode(llm=llm, db=db, k=k)
        self.consistency_check_node = ConsistencyCheckNode(llm=llm)

        # グラフの作成
        self.graph = self._create_graph()

    def _create_graph(self) -> StateGraph:
        # グラフの初期化
        workflow = StateGraph(State)

        # ノードの追加
        workflow.add_node("analyze_scripts", self._analyze_scripts)
        workflow.add_node("generate_sections", self._generate_sections)
        workflow.add_node("generate_document", self._generate_document)
        workflow.add_node("check_consistency", self._check_consistency)

        # エントリーポイントの追加
        workflow.set_entry_point("analyze_scripts")

        # ノード間のエッジの追加
        workflow.add_edge("analyze_scripts", "generate_sections")
        workflow.add_edge("generate_sections", "generate_document")
        workflow.add_edge("generate_document", "check_consistency")
        
        # 条件付きエッジの追加
        workflow.add_conditional_edges(
            "check_consistency",
            lambda state: not state.check_result and state.iteration < self.maximum_iteration,
            {True: "generate_sections", False: END}
        )

        # グラフのコンパイル
        return workflow.compile()
    
    def run(self, source_files: list[str]) -> GeneratedDocument:
        # Stateの初期化
        initial_state = State(source_files=source_files,
                              script_analysis_results=ScriptAnalysisResults(),
                              sections=Sections(title="ドキュメント", sections=[]),
                              document=GeneratedDocument(title="ドキュメント", documents=[]),
                              check_result=False, 
                              feedback="", 
                              iteration=0)
        
        # グラフの実行
        final_state = self.graph.invoke(initial_state)

        return final_state["document"]
    
    # ScriptAnalysisNode
    def _analyze_scripts(self, state: State):
        handle_proxy_request()  # プロキシ設定を自動で切り替える処理
        print("ソースコードの解析を開始します...")
        print(f"対象ファイル数: {len(state.source_files)}")

        script_analysis_result = self.script_analysis_node.run(state.source_files)
        return {
             "script_analysis_results": script_analysis_result
        }
    
    # DocumentOrchestrationNode
    def _generate_sections(self, state: State):
        handle_proxy_request()  # プロキシ設定を自動で切り替える処理
        if state.iteration < 1:
            print("ドキュメントの構成を作成しています...")
        else:
            print("再度ドキュメントの構成を見直しています...")
            print(f"試行回数: {state.iteration} / {self.maximum_iteration}")

        sections = self.document_orchestration_node.run(
            summaries=state.script_analysis_results,
            feedback=state.feedback, 
            latest_sections=state.sections
        )
        return {
            "sections": sections
        }
    
    # DocumentGenerationNode
    def _generate_document(self, state: State):
        handle_proxy_request()  # プロキシ設定を自動で切り替える処理
        print("ドキュメントを作成しています...")
        self.document_generation_node.set_summaries(state.script_analysis_results)
        document = self.document_generation_node.run(state.sections)
        return {
            "document": document
        }
    
    # ConsistencyCheckNode
    def _check_consistency(self, state: State):
        handle_proxy_request()  # プロキシ設定を自動で切り替える処理
        print("生成したドキュメントをチェックしています")
        evaluation_result = self.consistency_check_node.run(state.document)
        return {
            "check_result": evaluation_result.result,
            "feedback": evaluation_result.feedback,
            "iteration": state.iteration + 1
        }
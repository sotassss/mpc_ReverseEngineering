from langchain_core.prompts import ChatPromptTemplate

from src.model_types import ScriptAnalysisResults, Sections

class DocumentOrchestrationNode:
    def __init__(self, llm):
        self.llm = llm.with_structured_output(Sections)
        
    def run(self, summaries: ScriptAnalysisResults, feedback: str, latest_sections: Sections) -> Sections:
        """
        各スクリプトの要約から最終的に生成するドキュメントの章構成を決定するノード
        Args:
            summaries: 要約結果のリスト
            feedback: ConsistencyChechNodeから返されたフィードバック
            latest_sections: 最後に生成された章構成
        Returns:
            生成されたセクション構成
        """
        # 各スクリプトの要約結果を連結
        summaries_text = "\n\n".join([
            f"ファイル名: {s.file_path}\n内容: {s.short_summary}"
            for s in summaries
        ])

        messages = [
            (
                "system",
                "あなたは仕様書の編集者です。あなたの役割は仕様書の目次を作成することです。"
            ),
            (
                "human",
                "以下は仕様書を作成するシステムのファイル名とその説明です。\n\n"
                "{summaries}\n\n"
                "上記をもとに詳細で包括的な仕様書の目次を作成してください。\n"
                "「センシティブなファイルのため内容は非表示です。」と書かれているものは、推測をせずに、非表示としてください。\n\n"
                "一般的な内容を推測して記述してはいけません。\n\n"
                "システム的側面とビジネス的側面など多角的な視野から必要なセクションをリストアップして下さい。\n"
                "ただし、ソースコードの内容のみでは十分な情報が得られないようなセクションは含めないようにして下さい。\n"
                "また、付録と参考文献のセクションは作成しないでください。\n\n"
                "また、各セクションの担当編集者にわかりやすいように各セクションでどのような内容を記載すべきかについての説明も加えるようにしてください。\n"
                "また、最終的なドキュメントのタイトルも考えるようにしてください。"
            )
        ]

        # すでにフィードバックがある場合はフィードバックに応じて生成しなおすようにプロンプトを変更
        if feedback:
            messages.append(
                (
                    "ai",
                    "{latest_sections}"
                )
            )
            messages.append(
                (
                    "human",
                    "以下のフィードバックに基づいてさらに修正を行ってください\n\n"
                    "{feedback}"
                )
            )
        prompt = ChatPromptTemplate.from_messages(
            messages
        )

        chain = prompt | self.llm

        return chain.invoke({"summaries": summaries_text, "feedback": feedback, "latest_sections": latest_sections})
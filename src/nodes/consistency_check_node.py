from langchain_core.prompts import ChatPromptTemplate

from src.model_types import EvaluationResult, GeneratedDocument

class ConsistencyCheckNode:
    def __init__(self, llm):
        self.llm = llm.with_structured_output(EvaluationResult)

    def run(self, documents: GeneratedDocument) -> EvaluationResult:
        """
        完成したドキュメントについて、内容の可否を評価し不適切であればフィードバックを返すノード
        Args:
            documents: 生成されたドキュメント
        Returns:
            評価結果とフィードバック
        """
        prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "あなたは仕様書の編集者です。あなたの担当業務は仕様書のチェックです。"
                ),
                (
                    "human",
                    "以下のドキュメントが仕様書として適切かどうか判断してください。\n\n"
                    "その際、特に以下の観点に注意してください"
                    "1. ドキュメント全体を通して表現や内容が一貫しているかどうか\n"
                    "2. 内容が漏れなくかぶりなく、十分な内容から構成されているかどうか\n"
                    "3. 不要な議論や主張が含まれていないかどうか\n\n"
                    "内容が適切であると判断できる場合にTrueとその理由を返してください。\n"
                    "不適切であると判断した場合にはFalseと修正ポイントを含めたフィードバックを返してください。\n\n"
                    "以下評価対象のドキュメントです。"
                    "{document}"
                )
            ]
        )

        # chainの定義
        chain = prompt | self.llm

        return chain.invoke({"document": "\n\n".join(documents.documents)})
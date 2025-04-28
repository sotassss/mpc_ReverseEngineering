from typing import List
from pydantic import BaseModel, Field
from typing import Literal


# LLMの出力制限用のデータモデル定義
class ScriptAnalysisResult(BaseModel):
    file_path: str = Field(..., description="ファイルパス")
    detail_explanation: str = Field(..., description="詳細な説明")
    short_summary: str = Field(..., description="簡潔な要約")

class ScriptAnalysisResults(BaseModel):
    summaries: List[ScriptAnalysisResult] = Field(..., default_factory=list, description="ソースコードの要約のリスト")

class Section(BaseModel):
    section_name: str = Field(..., description="仕様書の章の名前")
    section_description: str = Field(..., description="仕様書の章の説明")

class Sections(BaseModel):
    title: str = Field(..., description="仕様書のタイトル")
    sections: List[Section] = Field(..., default_factory=list, description="仕様書の章のリスト")

class GeneratedDocument(BaseModel):
    title: str = Field(..., description="仕様書のタイトル")
    documents: List[str] = Field(..., default_factory=list, description="ドキュメントのリスト")

class EvaluationResult(BaseModel):
    result: bool = Field(..., description="内容が十分かどうかの判定結果")
    feedback: str = Field(..., description="セクション作成者へのフィードバック")

class RoutingDecision(BaseModel):
    decision: Literal["全体を見る必要あり", "部分的な情報で十分"] = Field(description="セクションの処理方法の判断")
    reasoning: str = Field(description="判断の理由説明")

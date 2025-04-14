from typing import List
from pydantic import BaseModel, Field

from src.model_types import (
    ScriptAnalysisResults,
    Sections,
    GeneratedDocument
)

# グラフのステートを定義
class State(BaseModel):
    source_files: List[str] = Field(..., default_factory=list, description="ソースコードのファイルパスのリスト")
    script_analysis_results: ScriptAnalysisResults = Field(description="ファイル分析結果のリスト")
    sections: Sections = Field(
        ..., description="セクションのリスト"
    )
    document: GeneratedDocument = Field(description="最終的なドキュメント")
    iteration: int = Field(default=0, description="ドキュメントの生成回数")
    check_result: bool = Field(description="ドキュメントの評価結果")
    feedback: str = Field(description="ドキュメントの評価結果のフィードバック")
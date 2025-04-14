from typing import List
from pydantic import BaseModel, Field
import os
from src.nodes.document_orchestration_node import DocumentOrchestrationNode

from langchain_core.prompts import ChatPromptTemplate
from langchain_openai import ChatOpenAI
import json

os.environ["OPENAI_API_KEY"] = "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"
llm = ChatOpenAI(model="gpt-4o-mini")
don = DocumentOrchestrationNode(llm)


inp = [
    {"file_name": "model_type.py",
     "short_summary": "エージェントのデータタイプを定義する"},
     {"file_name": "nodes.py",
     "short_summary": "エージェントのノードを定義する"},
     {"file_name": "wqrkflow.py",
     "short_summary": "エージェントのワークフローを実行する"}
]

print(don.run(inp))
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import PydanticOutputParser
from langchain_core.runnables import RunnableLambda
from pydantic import BaseModel, Field, field_validator
from dotenv import load_dotenv

load_dotenv()

model="gpt-4o-mini"
temperature=1.0

llm = ChatOpenAI(model=model, temperature=temperature)

class Joke(BaseModel):
    setup: str = Field(description="ジョークのセットアップの部分")
    punchline:str = Field(description="ジョークを解決する答え")

    # @field_validator('setup')
    # def question_ends_with_question_mark(cls, field):
    #     if not (field.endswith("?") or field.endswith("？")):
    #         raise ValueError("setup must end with a question mark")
    #     return field
    
parser = PydanticOutputParser(pydantic_object=Joke)

# プロンプトテンプレートの定義
prompt = ChatPromptTemplate.from_messages([
    ("system", "ユーザのクエリに答えてください。\n{format_instructions}"),
    ("user", "{query}")
])
prompt = prompt.partial(format_instructions=parser.get_format_instructions())

# チェーンの作成（RunnableMap を使うと柔軟）
chain = (
    {"query": RunnableLambda(lambda x: x)}  # 入力をそのまま渡す
    | prompt
    | llm
    | parser
)

# 実行
joke_query = "動物を使わない日本語のユーモアあふれるジョークを1つ教えてください。"
result = chain.invoke(joke_query)

# 結果の表示
print("Setup:", result.setup)
print("Punchline:", result.punchline)
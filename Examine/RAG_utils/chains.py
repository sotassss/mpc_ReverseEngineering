import json
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.embeddings import OpenAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnableLambda, RunnablePassthrough

# LLM、埋め込みモデルを初期化
llm = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")

# グローバル変数
db = None
full_document_content = ""

# RAG用のチェーン
def create_rag_chain():
    def retrieve_context(query):
        search_results = db.similarity_search(query, k=2)
        context = "\n".join([result.page_content for result in search_results])
        return {"context": context, "query": query}
    
    retriever = RunnableLambda(retrieve_context)
    
    prompt = ChatPromptTemplate.from_messages([
        (
            "system",
            "あなたはチャットボットの回答作成者です。与えられた文脈に基づいて質問に回答してください。"
            "文脈に情報がない場合は「その情報は文脈にありません」と回答してください。"
        ),
        (
            "human",
            "次の文脈に基づいて質問に回答してください。\n\n"
            "文脈: {context}\n\n"
            "質問: {query}\n"
        )
    ])
    
    chain = retriever | prompt | llm
    return chain

# LLM用のチェーン
def create_llm_chain():
    prompt = ChatPromptTemplate.from_messages([
        (
            "system",
            "あなたはチャットボットの回答作成者です。"
        ),
        (
            "human",
            "次の質問に答えてください。\n\n"
            "質問: {query}\n"
        )
    ])
    
    chain = prompt | llm
    return chain

# LC用のチェーン
def create_lc_chain():
    def add_full_context(query_dict):
        return {"full_context": full_document_content, "query": query_dict["query"]}
    
    prompt = ChatPromptTemplate.from_messages([
        (
            "system",
            "あなたはチャットボットの回答作成者です。与えられた文書全体を文脈として質問に回答してください。"
            "文脈に情報がない場合は「その情報は文書内にありません」と回答してください。"
        ),
        (
            "human",
            "次の文書全体を読み、質問に回答してください。\n\n"
            "文書: {full_context}\n\n"
            "質問: {query}\n"
        )
    ])
    
    chain = RunnablePassthrough() | RunnableLambda(add_full_context) | prompt | llm
    return chain

# ルーター用のチェーン
def create_router_chain():
    router_prompt = ChatPromptTemplate.from_messages([
        (
            "system",
            """
                与えられた質問を分析し、適切な回答方法を決定してください。\n\n
                回答方法には以下の3つがあります:\n
                1. RAG - PDFドキュメントの関連部分のみを検索して回答する方法。特定のトピックや情報に焦点を当てた質問に適しています。\n
                2. LLM - モデルの一般知識に基づいて回答する方法。一般的な知識や事実に関する質問に適しています。\n
                3. LC - Large Context、PDFドキュメント全体を文脈として使用する方法。文書全体の概要や複数セクションにまたがる情報を必要とする複雑な質問に適しています。\n\n
                以下の形式でJSONを返してください:\n\
                {{
                "reasoning": "質問の分析と回答方法を選んだ理由",
                "route": "RAG または LLM または LC"
                }}
                ・特定の情報や単一のトピックに関する質問はRAGを選択してください。\n
                ・文書全体の要約や複数セクションにまたがる複雑な分析を必要とする質問はLCを選択してください。\n
                ・PDFに含まれていない一般的な知識や事実に関する質問はLLMを選択してください。\n
            """
        ),
        (
            "human",
            "質問: {query}"
        )
    ])
    
    def parse_router_response(router_response):
        try:
            return json.loads(router_response.content)
        except json.JSONDecodeError:
            content_upper = router_response.content.upper()
            if "RAG" in content_upper:
                return {"reasoning": "自動抽出", "route": "RAG"}
            elif "LC" in content_upper or "LARGE CONTEXT" in content_upper:
                return {"reasoning": "自動抽出", "route": "LC"}
            else:
                return {"reasoning": "自動抽出", "route": "LLM"}
    
    chain = router_prompt | llm | RunnableLambda(parse_router_response)
    return chain
import json
from langchain.embeddings import OpenAIEmbeddings
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.embeddings import OpenAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnableLambda, RunnablePassthrough
from langchain_chroma import Chroma
from typing import Dict, Any

load_dotenv()

# LLM、埋め込みモデルを初期化
llm = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")

# (1)RAGを使ったチェーンの作成
def create_rag_chain(db: Chroma, top_k: int = 2) -> str:

    def retrieve_context(query):
        search_results = db.similarity_search(query, k=top_k)
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



# (2)LLMを用いたチェーンの作成
def create_llm_chain() -> str:
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



# (3)Large Context（LC）を用いたチェーンの作成
def create_lc_chain(full_document_content: str) -> str:
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



# どの回答方法を利用するかを選択する関数
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
        


# ルーターでルートを決定し、適切なチェーンに接続
def create_self_routing_chain(db: Chroma, full_document_content: str) -> str:
    router_chain = create_router_chain()
    rag_chain = create_rag_chain(db)
    llm_chain = create_llm_chain()
    lc_chain = create_lc_chain(full_document_content)
    
    def route_to_chain(inputs):
        query = inputs["query"]
        route_info = router_chain.invoke({"query": query})
        
        route = route_info["route"].upper()
        reasoning = route_info["reasoning"]
        
        if route == "RAG":
            answer = rag_chain.invoke(query)
            return {"answer": answer, "method": "RAG (関連部分検索)", "reasoning": reasoning}
        
        elif route == "LC":
            answer = lc_chain.invoke({"query": query})
            return {"answer": answer, "method": "LC (文書全体文脈)", "reasoning": reasoning}
        
        else:  # LLM
            answer = llm_chain.invoke({"query": query})
            return {"answer": answer, "method": "LLM (一般知識)", "reasoning": reasoning}
    
    chain = RunnablePassthrough() | RunnableLambda(route_to_chain)
    return chain



# メインの処理
def answer_with_self_route(query: str, db, full_document_content) -> Dict[str, Any]:
    chain = create_self_routing_chain(db, full_document_content)
    result = chain.invoke({"query": query})
    
    return {
        "query": query,
        "answer": result["answer"],
        "method": result["method"],
        "reasoning": result["reasoning"]
    }


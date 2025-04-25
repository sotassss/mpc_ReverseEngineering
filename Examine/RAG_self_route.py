# モジュールのインポート
import json
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.embeddings import OpenAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate
from typing import Literal, Dict, Any

from RAG_utils.pdf_loader import pdf_loader

# 環境変数を読み込む
load_dotenv()

# LLM、埋め込みモデルを初期化
llm = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")

# グローバル変数
db = None
full_document_content = ""  



# (1) RAGを使った回答
def answer_RAG(query: str, top_k: int = 2) -> str:
    global db
    
    if db is None:
        return "エラー: データベースが初期化されていません"
    
    search_results = db.similarity_search(query, k=top_k)
    
    # 検索結果からドキュメントをまとめる
    context = "\n".join([result.page_content for result in search_results])
    
    # LLMに質問を投げる
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
    chain = prompt | llm
    answer = chain.invoke({"context": context, "query": query})
    return answer



# (2) LLMを用いた回答
def answer_LLM(query: str) -> str:
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
    answer = chain.invoke({"query": query})
    return answer



# (3) Large Context（LC）を用いた回答
def answer_LC(query: str) -> str:
    global full_document_content
    
    if not full_document_content:
        return "エラー: ドキュメントが読み込まれていません"
    
    # LLMに質問を投げる（文書全体を文脈として使用）
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
    chain = prompt | llm
    answer = chain.invoke({"full_context": full_document_content, "query": query})
    return answer


# どの回答方法を利用するかを選択する関数
def classify_query(query: str) -> Dict[str, Any]:
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
    
    chain = router_prompt | llm
    response = chain.invoke({"query": query})
    
    # モデルの応答からJSONを取得
    try:
        response_content = response.content
        return json.loads(response_content)
    except json.JSONDecodeError:
        # JSON解析が失敗した場合、テキストから必要な情報を抽出
        print("JSON解析に失敗しました。テキストから情報を抽出します。")
        content_upper = response.content.upper()
        if "RAG" in content_upper:
            return {"reasoning": "自動抽出", "route": "RAG"}
        elif "LC" in content_upper or "LARGE CONTEXT" in content_upper:
            return {"reasoning": "自動抽出", "route": "LC"}
        else:
            return {"reasoning": "自動抽出", "route": "LLM"}
        


# メインの回答生成関数（セルフルーティングを使用）
def answer_with_self_route(query: str) -> Dict[str, Any]:
    # 質問のタイプを分類
    classification = classify_query(query)
    route = classification.get("route", "LLM")
    reasoning = classification.get("reasoning", "理由不明")
    
    # 分類に基づいて適切な回答方法を選択
    if route.upper() == "RAG":
        answer = answer_RAG(query)
        method = "RAG (関連部分検索）"
    elif route.upper() == "LC":
        answer = answer_LC(query)
        method = "LC (文書全体文脈）"
    else:
        answer = answer_LLM(query)
        method = "LLM (一般知識）"
    
    # 結果を返す
    return {
        "query": query,
        "answer": answer,
        "method": method,
        "reasoning": reasoning
    }



# 実行
if __name__ == "__main__":
    pdf_path = "sample2.pdf"
    query = "江戸幕府が成立したのはいつ"
    
    # PDFをロード
    pdf_loader(pdf_path)
    
    # セルフルーティングで回答生成
    result = answer_with_self_route(query)
    
    # 結果を表示
    print(f"質問: {result['query']}")
    print(f"回答方法: {result['method']}")
    print(f"理由: {result['reasoning']}")
    print(f"回答: {result['answer'].content}")
else:
    # 対話モードでの実行用
    pdf_path = "sample2.pdf"
    pdf_loader(pdf_path)
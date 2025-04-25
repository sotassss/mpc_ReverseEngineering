# モジュールのインポート
import os
import shutil
import time
from uuid import uuid4
from pathlib import Path
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.document_loaders import PyPDFLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.embeddings import OpenAIEmbeddings
from langchain.schema import Document
from langchain_chroma import Chroma
from langchain_core.prompts import ChatPromptTemplate
from typing import Literal, Dict, Any

# 環境変数を読み込む
load_dotenv()

# LLM、埋め込みモデルを初期化
llm = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")

# グローバル変数
db = None

# PDFをデータベースに保存
def pdf_loader(pdf_path: str, persist_directory: str = "./chroma_langchain_db") -> Chroma:
    global db
    
    # chromaデータベースを初期化
    CHROMA_DIR = "./chroma_langchain_db"
    try:
        if os.path.exists(CHROMA_DIR):
            shutil.rmtree(CHROMA_DIR)
            print(f"ベクトルストア {CHROMA_DIR} を初期化しました")
    except PermissionError:
        print("ファイル削除エラー: 別のプロセスがファイルを使用中です")
        # 別のディレクトリ名を使用
        persist_directory = f"{persist_directory}_{int(time.time())}"
        print(f"新しいディレクトリを使用します: {persist_directory}")
    
    # ベクトルストアを構築
    db = Chroma(
        collection_name="example_collection",
        embedding_function=embeddings,
        persist_directory=persist_directory,
    )
    
    # PDF読み込み
    loader = PyPDFLoader(pdf_path)
    documents = loader.load()
    print(f"Loaded {len(documents)} pages from {pdf_path}")
    
    # 分割
    splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    split_docs = splitter.split_documents(documents)
    print(f"Split into {len(split_docs)} chunks")
    
    # 元のメタデータを保持しつつ、ファイルパスを追加
    docs = []
    for doc in split_docs:
        # 元のメタデータをコピー
        metadata = doc.metadata.copy() if hasattr(doc, 'metadata') and doc.metadata else {}
        # ファイルパスを追加
        metadata["file_path"] = pdf_path
        docs.append(Document(page_content=doc.page_content, metadata=metadata))
    
    # UUID生成
    uuids = [str(uuid4()) for _ in range(len(docs))]
    
    # ベクトルストアに追加
    db.add_documents(documents=docs, ids=uuids)
    # db.persist()  # データベースを永続化が必要な場合はコメントを外す
    print(f"{pdf_path}が保存されました")
    
    return db

# RAGを使った回答
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

# LLMを用いた回答
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

def classify_query(query: str) -> Dict[str, Any]:
    router_prompt = ChatPromptTemplate.from_messages([
        (
            "system",
            """
                与えられた質問を分析し、適切な回答方法を決定してください。

                回答方法には以下の2つがあります:
                1. RAG - PDFドキュメントに基づいて回答する方法。ドキュメントの内容や具体的な情報を問う質問に適しています。
                2. LLM - モデルの一般知識に基づいて回答する方法。一般的な知識や事実に関する質問に適しています。

                以下の形式でJSONを返してください:
                {{
                "reasoning": "質問の分析と回答方法を選んだ理由",
                "route": "RAG または LLM"
                }}

                PDFに含まれそうな質問や具体的なドキュメントの内容に関する質問はRAGを選択し、
                歴史的事実や一般的な知識に関する質問はLLMを選択してください。
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
    import json
    try:
        response_content = response.content
        return json.loads(response_content)
    except json.JSONDecodeError:
        # JSON解析が失敗した場合、テキストから必要な情報を抽出
        print("JSON解析に失敗しました。テキストから情報を抽出します。")
        if "RAG" in response.content.upper():
            return {"reasoning": "自動抽出", "route": "RAG"}
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
        method = "RAG(ドキュメント検索）"
    else:
        answer = answer_LLM(query)
        method = "LLM(一般知識）"
    
    # 結果を返す
    return {
        "query": query,
        "answer": answer,
        "method": method,
        "reasoning": reasoning
    }

# 実行例
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
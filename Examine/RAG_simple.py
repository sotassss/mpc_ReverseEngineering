# モジュールのインポート
import os
import shutil
from pathlib import Path
from uuid import uuid4
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.document_loaders import PyPDFLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.embeddings import OpenAIEmbeddings
from langchain.schema import Document
from langchain_chroma import Chroma
from langchain_core.prompts import ChatPromptTemplate

# LLM、埋め込みモデルを初期化
llm = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")

# PDFをデータベースに保存
def pdf_loader(pdf_path: str, persist_directory: str = "./chroma_langchain_db") -> Chroma:
    global db

    # chromaデータベースを初期化
    CHROMA_DIR = "./chroma_langchain_db"
    if os.path.exists(CHROMA_DIR):
        shutil.rmtree(CHROMA_DIR)

    # ベクトルストアを構築
    embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
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
    # db.persist()  # データベースを永続化
    print(f"{pdf_path}が保存されました")
    
    return db 


# RAGの実装
def answer_RAG(query: str, top_k: int = 2) -> str:
    global db
    search_results=db.similarity_search(query, k=top_k)

    # 検索結果からドキュメントをまとめる
    context = "\n".join([result.page_content for result in search_results])

    # LLMに質問を投げる
    prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "あなたはチャットボットの回答作成者です。"
                ),
                (
                    "human",
                    "次の文脈に基づいて質問に回答してください。\n\n"
                    "質問: {context}\n\n"
                    "質問：{query}\n"
                )
            ]
    )
    chain = prompt | llm
    answer = chain.invoke({"context": context, "query": query})
    return answer


# LLMを用いた回答
def answer_LLM(query: str) -> str: 
    prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "あなたはチャットボットの回答作成者です。"
                ),
                (
                    "human",
                    "次の質問に答えてください。\n\n"
                    "質問: {query}\n"
                )
            ]
    )
    chain = prompt | llm
    answer = chain.invoke({"query": query})
    return answer

# queryを設定して実行
pdf_path="sample2.pdf"
query="江戸幕府が成立したのはいつ"

pdf_loader(pdf_path)
answer=answer_RAG(query)
print(answer.content)
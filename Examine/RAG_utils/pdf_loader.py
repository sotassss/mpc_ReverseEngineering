import os  
import shutil
import time
from uuid import uuid4
from langchain.document_loaders import PyPDFLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.embeddings import OpenAIEmbeddings
from langchain.schema import Document
from langchain_chroma import Chroma

def pdf_loader(pdf_path: str, persist_directory: str = "./chroma_langchain_db") -> Chroma:
    global db, full_document_content
    embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
    
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
    
    # Large Context用に全文を結合して保存
    full_document_content = "\n\n".join([doc.page_content for doc in documents])
    
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
    
    return db, full_document_content
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_chroma import Chroma
from src.agent import ReverseEngine
from dotenv import load_dotenv

load_dotenv()

def main():
    source_files = [
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample1.py",
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample2.py",
    ]
    # ソースコードのリストを入力

    llm = ChatOpenAI(model="gpt-4o-mini")
    embeddings = OpenAIEmbeddings(model="text-emmbedding-3-small")
    db = Chroma(
        collection_name="example_collection",
        embedding_function=embeddings,
        persist_directory="./chroma_langchain_db",
    )

    agent = ReverseEngine(llm=llm, db=db, k=10, maximum_iteration=3)

    output = agent.run(source_files=source_files) # ドキュメントが出力


if __name__ == "__main__":
    main()
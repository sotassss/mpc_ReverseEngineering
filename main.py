from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_chroma import Chroma
from src.agent import ReverseEngine
from dotenv import load_dotenv
from src.utils.write_output_pdf import write_output_pdf
from src.utils.write_output_md import write_output_md



load_dotenv()

def main():
    # ソースコードのリストを入力
    source_files = [
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample1.py",
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample2.py",
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample3.py",
    ]
    
    llm = ChatOpenAI(model="gpt-4o-mini")
    embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
    db = Chroma(
        collection_name="example_collection",
        embedding_function=embeddings,
        persist_directory="./chroma_langchain_db",
    )

    agent = ReverseEngine(llm=llm, db=db, k=10, maximum_iteration=3)

    output = agent.run(source_files=source_files) # ドキュメントが出力

    write_output_md(output) # ドキュメントをMarkdown形式で保存
    write_output_pdf(output) # ドキュメントをPDF形式で保存


if __name__ == "__main__":
    main()
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_chroma import Chroma
from src.agent import ReverseEngine
from dotenv import load_dotenv
import os
import shutil
from src.utils.collect_all_files import collect_all_files
from src.utils.write_output_md import write_output_md
from src.utils.md_to_pdf import convert_md_to_pdf



def main():
    load_dotenv()
    
    # ソースコードが格納されているフォルダのパス
    # source_folder = "code/code_git_ROS"
    # source_folder = "code/code_git_langchain"
    # source_folder = "code/code_sample_python"
    source_folder = "code/test"

    # chromaデータベースを初期化
    CHROMA_DIR = "./chroma_langchain_db"
    if os.path.exists(CHROMA_DIR):
        shutil.rmtree(CHROMA_DIR)
    source_files = collect_all_files(source_folder) 
    
    llm = ChatOpenAI(model="gpt-4.1-nano-2025-04-14")
    # llm = ChatOpenAI(model="gpt-4o-mini")
    embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
    db = Chroma(
        collection_name="example_collection",
        embedding_function=embeddings,
        persist_directory="./chroma_langchain_db",
    )

    agent = ReverseEngine(llm=llm, db=db, k=10, maximum_iteration=3, wait_time=0.5)

    output = agent.run(source_files=source_files) 

    # Markdownファイルとして保存
    md_file = write_output_md(output)

    # PDFファイルとして保存
    convert_md_to_pdf(md_file)

if __name__ == "__main__":
    main()
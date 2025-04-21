from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_chroma import Chroma
from src.agent import ReverseEngine
from dotenv import load_dotenv
from src.utils.collect_all_files import collect_all_files
from src.utils.write_output_md import write_output_md


load_dotenv()

def main():
    # ソースコードが格納されているフォルダのパス
    source_folder = "code/code_git"
    # source_folder = "code/code_sample_python"

    source_files = collect_all_files(source_folder) 
    
    llm = ChatOpenAI(model="gpt-4o-mini")
    embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
    db = Chroma(
        collection_name="example_collection",
        embedding_function=embeddings,
        persist_directory="./chroma_langchain_db",
    )

    agent = ReverseEngine(llm=llm, db=db, k=10, maximum_iteration=3)

    output = agent.run(source_files=source_files) # ドキュメントの出力

    write_output_md(output) # ドキュメントをMarkdown形式で保存


if __name__ == "__main__":
    main()
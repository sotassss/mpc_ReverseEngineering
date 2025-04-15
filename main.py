from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_chroma import Chroma
from src.agent import ReverseEngine
import os
from datetime import datetime
from dotenv import load_dotenv

load_dotenv()

def main():
    # ソースコードのリストを入力
    source_files = [
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample1.py",
    "C:/Users/1109685/Documents/Maxi/code_sample_python/sample2.py",
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

    # ファイル名に日付と時間を追加
    current_time = datetime.now().strftime("%Y_%m_%d_%H%M")
    output_filename = f"Document_{current_time}.txt"

    # 保存先ディレクトリの確認と作成
    save_directory = "./output/"
    os.makedirs(save_directory, exist_ok=True)

    # 完成資料をファイルに保存
    output_filepath = os.path.join(save_directory, output_filename)
    with open(output_filepath, "w", encoding="utf-8") as file:
        file.write(output)

    print(f"ドキュメントが {output_filepath} に保存されました。")


if __name__ == "__main__":
    main()
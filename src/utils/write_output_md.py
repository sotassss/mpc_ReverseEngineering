import os
from datetime import datetime

def write_output_md(output):
    # ファイル名に日時を付ける
    current_time = datetime.now().strftime("%Y_%m_%d_%H%M")
    output_filename = f"Document_{current_time}.md"

    # 保存先ディレクトリ作成
    save_directory = "./output/"
    os.makedirs(save_directory, exist_ok=True)

    output_filepath = os.path.join(save_directory, output_filename)

    with open(output_filepath, "w", encoding="utf-8") as file:
        file.write(f"# {output.title}\n\n")
        for i, doc in enumerate(output.documents, 1):
            file.write(f"## セクション {i}\n\n{doc}\n\n")

    print(f"ドキュメントが {output_filepath} に保存されました。")

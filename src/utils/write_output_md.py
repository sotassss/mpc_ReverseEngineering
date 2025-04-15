import os
from datetime import datetime

def write_output_md(output):
    # ファイル名に日時を付ける
    current_date = datetime.now().strftime("%m_%d")
    current_time = datetime.now().strftime("%Y_%m_%d_%H%M")
    output_folder = f"Output_{current_date}"
    output_filename = f"Document_{current_time}.md"
  
    # 保存先ディレクトリ作成
    save_directory = os.path.join("./Output", output_folder)
    os.makedirs(save_directory, exist_ok=True)
    
    output_filepath = os.path.join(save_directory, output_filename)
    
    # 目次を作成
    toc = [f"# {output.title}\n\n## 目次\n"]
    doc_titles = []
    
    # 各ドキュメントの最初の行（タイトル）を抽出
    for i, doc in enumerate(output.documents, 1):
        # 最初の行をタイトルとして抽出
        if doc:
            lines = doc.split('\n')
            title = lines[0].strip()
            # '#' で始まる場合は Markdown のタイトル形式を処理
            if title.startswith('#'):
                title = title.lstrip('#').strip()
            doc_titles.append((i, title))
    
    # 目次項目を追加
    for i, title in doc_titles:
        toc.append(f"{i}. [{title}](#{i}-{title.lower().replace(' ', '-')})\n")
    
    toc.append("\n---\n\n")
    
    # 各ドキュメントの内容を追加
    content = []
    for i, doc in enumerate(output.documents, 1):
        if doc:
            # セクション番号を追加
            title = doc_titles[i-1][1]
            # エラーが発生していた行の修正
            doc_content = doc.split('\n', 1)[1] if '\n' in doc else ''
            content.append(f"## {i}. {title}\n\n{doc_content}\n\n")
    
    # 目次と内容を結合して書き込み
    with open(output_filepath, "w", encoding="utf-8") as file:
        file.write("".join(toc))
        file.write("".join(content))
    
    print(f"ドキュメントが {output_filepath} に保存されました。")
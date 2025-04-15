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
    
    # 目次を作成 - タイトルを大きく（H1）、目次ヘッダーを（H2）に
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
    
    # 目次項目を追加（番号を除去）
    for i, title in doc_titles:
        # タイトルからすでに含まれている数字や記号を取り除く
        clean_title = title
        # 先頭の数字とピリオドのパターン（例：「1. 」）を削除
        if clean_title.strip() and clean_title[0].isdigit():
            parts = clean_title.strip().split('.', 1)
            if len(parts) > 1 and parts[0].isdigit():
                clean_title = parts[1].strip()
        
        toc.append(f"{i}. [{clean_title}](#{i}-{clean_title.lower().replace(' ', '-').replace('.', '').replace('(', '').replace(')', '')})\n")
    
    toc.append("\n---\n\n")
    
    # 各ドキュメントの内容を追加
    content = []
    for i, doc in enumerate(output.documents, 1):
        if doc:
            # セクション番号を追加
            title = doc_titles[i-1][1]
            
            # タイトルから既存の番号を削除
            clean_title = title
            if clean_title.strip() and clean_title[0].isdigit():
                parts = clean_title.strip().split('.', 1)
                if len(parts) > 1 and parts[0].isdigit():
                    clean_title = parts[1].strip()
            
            # ドキュメント内容を処理
            doc_lines = doc.split('\n')
            processed_lines = []
            
            # 先頭行（タイトル）をスキップ
            skip_first = True
            
            for line in doc_lines:
                if skip_first:
                    skip_first = False
                    continue
                
                # 他のヘッダーを1レベル下げる（H2→H3、H3→H4など）
                if line.strip().startswith('#'):
                    header_level = 0
                    for char in line:
                        if char == '#':
                            header_level += 1
                        else:
                            break
                    
                    # ヘッダーレベルを1つ増やす（より小さく）
                    if header_level >= 1:
                        new_header = '#' * (header_level + 1) + line[header_level:]
                        processed_lines.append(new_header)
                    else:
                        processed_lines.append(line)
                else:
                    processed_lines.append(line)
            
            # セクションタイトルを H2 に（数字付き）
            content.append(f"## {i}. {clean_title}\n\n" + '\n'.join(processed_lines) + "\n\n")
    
    # 目次と内容を結合して書き込み
    with open(output_filepath, "w", encoding="utf-8") as file:
        file.write("".join(toc))
        file.write("".join(content))
    
    print(f"ドキュメントが {output_filepath} に保存されました。")
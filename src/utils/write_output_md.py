import os
from datetime import datetime
import re

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
    
    # 各ドキュメントのタイトルを適切に抽出
    for i, doc in enumerate(output.documents, 1):
        if doc:
            lines = doc.split('\n')
            first_line = lines[0].strip()
            
            # Markdownのタイトル形式を処理
            title = first_line
            if title.startswith('#'):
                # '#'記号を除去
                title = re.sub(r'^#+\s*', '', title)
            
            # 先頭の数字とピリオドのパターンを削除（例：「1. 」）
            title = re.sub(r'^\d+\.\s*', '', title)
            
            doc_titles.append((i, title))
    
    # 目次項目を追加
    for i, title in doc_titles:
        # 目次リンク用のアンカー生成
        anchor = title.lower().replace(' ', '-')
        # 特殊文字を削除
        anchor = re.sub(r'[^\w\-]', '', anchor)
        toc.append(f"{i}. [{title}](#{i}-{anchor})\n")
    
    toc.append("\n---\n\n")
    
    # 各ドキュメントの内容を追加
    content = []
    for i, doc in enumerate(output.documents, 1):
        if doc:
            title = doc_titles[i-1][1]
            
            # ドキュメント内容を処理
            doc_lines = doc.split('\n')
            processed_lines = []
            
            # 先頭行（タイトル）をスキップ
            first_line = True
            
            for line in doc_lines:
                if first_line:
                    first_line = False
                    continue
                
                # ヘッダーを調整（H1→H2、H2→H3など）
                header_match = re.match(r'^(#+)\s+(.*)', line)
                if header_match:
                    # 既存のヘッダーレベルを1つ増やす
                    hash_marks = header_match.group(1) + '#'
                    header_content = header_match.group(2)
                    processed_lines.append(f"{hash_marks} {header_content}")
                else:
                    processed_lines.append(line)
            
            # セクションのアンカー生成
            anchor = title.lower().replace(' ', '-')
            anchor = re.sub(r'[^\w\-]', '', anchor)
            
            # セクションタイトルを H2 に（アンカー付き）
            content.append(f"<h2 id=\"{i}-{anchor}\">{i}. {title}</h2>\n\n" + '\n'.join(processed_lines) + "\n\n")
    
    # 目次と内容を結合して書き込み
    with open(output_filepath, "w", encoding="utf-8") as file:
        file.write("".join(toc))
        file.write("".join(content))
    
    print(f"ドキュメントが {output_filepath} に保存されました。")
    return output_filepath
#!/usr/bin/env python3
# coding: utf-8

import os
import re
import argparse
import tempfile
import subprocess
import base64
from pathlib import Path
import markdown
from weasyprint import HTML
import uuid

def extract_mermaid_blocks(content):
    """Markdownコンテンツから全てのmermaidブロックを抽出します"""
    pattern = r'```mermaid\s*(.*?)\s*```'
    return re.findall(pattern, content, re.DOTALL)

def render_mermaid_to_svg(mermaid_code):
    """mermaidコードをSVGに変換します"""
    # 一時ファイルの作成
    tmp_dir = tempfile.gettempdir()
    mermaid_file = os.path.join(tmp_dir, f"mermaid_{uuid.uuid4()}.mmd")
    
    with open(mermaid_file, 'w', encoding='utf-8') as f:
        f.write(mermaid_code)
    
    svg_file = os.path.join(tmp_dir, f"mermaid_{uuid.uuid4()}.svg")
    
    # mermaid-cliを使ってSVGを生成
    try:
        subprocess.run(
            ["npx", "@mermaid-js/mermaid-cli", "-i", mermaid_file, "-o", svg_file],
            check=True, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE
        )
        
        # SVGファイルを読み込む
        with open(svg_file, 'r', encoding='utf-8') as f:
            svg_content = f.read()
        
        # 一時ファイルを削除
        os.remove(mermaid_file)
        os.remove(svg_file)
        
        return svg_content
    except subprocess.CalledProcessError as e:
        print(f"Error rendering mermaid: {e}")
        return None

def markdown_with_mermaid_to_pdf(input_file, output_file):
    """Markdownファイル（Mermaid図を含む）をPDFに変換します"""
    # Markdownファイルを読み込み
    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Mermaidブロックを抽出
    mermaid_blocks = extract_mermaid_blocks(content)
    
    # 各Mermaidブロックを処理
    for i, mermaid_code in enumerate(mermaid_blocks):
        svg_content = render_mermaid_to_svg(mermaid_code)
        if svg_content:
            # SVGをBase64エンコード
            svg_base64 = base64.b64encode(svg_content.encode('utf-8')).decode('utf-8')
            # Markdownの中のMermaidブロックをSVG画像に置き換え
            content = content.replace(
                f"```mermaid\n{mermaid_code}\n```", 
                f'<img src="data:image/svg+xml;base64,{svg_base64}" alt="Mermaid diagram" />'
            )
    
    # MarkdownをHTMLに変換
    html_content = markdown.markdown(content, extensions=['extra', 'codehilite'])
    
    # 適切なスタイルシートを追加
    styled_html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <style>
            body {{ 
                font-family: Arial, sans-serif; 
                line-height: 1.6;
                margin: 2em;
            }}
            img {{ max-width: 100%; }}
            pre {{ 
                background-color: #f5f5f5; 
                padding: 1em; 
                border-radius: 5px;
                overflow-x: auto;
            }}
            code {{ font-family: monospace; }}
            h1, h2, h3, h4, h5, h6 {{ color: #333; }}
        </style>
    </head>
    <body>
        {html_content}
    </body>
    </html>
    """
    
    # HTMLからPDFを生成
    HTML(string=styled_html).write_pdf(output_file)
    
    print(f"変換が完了しました。PDFファイルが保存されました: {output_file}")

def main():
    parser = argparse.ArgumentParser(description='MarkdownファイルとMermaid図をPDFに変換')
    parser.add_argument('input_file', help='入力Markdownファイルのパス')
    parser.add_argument('-o', '--output', help='出力PDFファイルのパス')
    
    args = parser.parse_args()
    
    input_path = Path(args.input_file)
    
    if not args.output:
        output_path = input_path.with_suffix('.pdf')
    else:
        output_path = Path(args.output)
    
    markdown_with_mermaid_to_pdf(input_path, output_path)

if __name__ == '__main__':
    main()
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
from pathlib import Path
import markdown2
import pdfkit
import tempfile
from bs4 import BeautifulSoup

def convert_md_to_pdf(input_file, output_file=None, style=None):
    # 出力ファイル名が指定されていない場合は入力ファイル名から生成
    if not output_file:
        input_path = Path(input_file)
        output_file = str(input_path.with_suffix('.pdf'))
    
    print(f"Converting {input_file} to {output_file}...")
    
    # wkhtmltopdfのパスを明示的に指定
    config = pdfkit.configuration(wkhtmltopdf=r'C:\Program Files\wkhtmltopdf\bin\wkhtmltopdf.exe')
    
    # Markdown ファイルを読み込む
    with open(input_file, 'r', encoding='utf-8') as f:
        md_content = f.read()
    
    # Markdown を HTML に変換（拡張機能付き）
    html_content = markdown2.markdown(
        md_content,
        extras=[
            'fenced-code-blocks',  # コードブロックサポート
            'tables',              # テーブルサポート
            'header-ids',          # ヘッダーにIDを追加
            'toc',                 # 目次サポート
            'footnotes',           # 脚注サポート
            'task_list',           # タスクリストサポート
            'code-friendly',       # コード内のアンダースコアを保持
            'smarty-pants',        # スマートな引用符や省略記号
            'strike',              # 取り消し線サポート
            'math',                # 数式サポート
        ]
    )
    
    # HTML 文書を解析
    soup = BeautifulSoup(html_content, 'html.parser')
    
    # コードブロックにスタイルを適用
    for code_block in soup.find_all('pre'):
        code_block['class'] = code_block.get('class', []) + ['code-block']
    
    # スタイルシートの読み込み
    custom_css = ""
    if style and os.path.exists(style):
        with open(style, 'r', encoding='utf-8') as f:
            custom_css = f.read()
    
    # HTML にヘッダーと基本的なスタイルを追加
    complete_html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <title>{os.path.basename(input_file)}</title>
        <style>
            body {{
                font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
                line-height: 1.6;
                padding: 20px;
                max-width: 800px;
                margin: 0 auto;
                color: #333;
            }}
            
            h1, h2, h3, h4, h5, h6 {{
                margin-top: 24px;
                margin-bottom: 16px;
                font-weight: 600;
                line-height: 1.25;
                color: #111;
            }}
            
            h1 {{ font-size: 2em; border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }}
            h2 {{ font-size: 1.5em; border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }}
            h3 {{ font-size: 1.25em; }}
            h4 {{ font-size: 1em; }}
            
            code {{
                font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
                background-color: #f6f8fa;
                border-radius: 3px;
                padding: 0.2em 0.4em;
                font-size: 85%;
            }}
            
            pre {{
                padding: 16px;
                overflow: auto;
                font-size: 85%;
                line-height: 1.45;
                background-color: #f6f8fa;
                border-radius: 3px;
                margin: 16px 0;
            }}
            
            pre code {{
                background-color: transparent;
                padding: 0;
                border-radius: 0;
            }}
            
            a {{
                color: #0366d6;
                text-decoration: none;
            }}
            
            a:hover {{
                text-decoration: underline;
            }}
            
            blockquote {{
                margin: 0;
                padding: 0 1em;
                color: #6a737d;
                border-left: 0.25em solid #dfe2e5;
            }}
            
            table {{
                border-collapse: collapse;
                width: 100%;
                margin: 16px 0;
            }}
            
            table th, table td {{
                padding: 6px 13px;
                border: 1px solid #dfe2e5;
            }}
            
            table tr {{
                background-color: #fff;
                border-top: 1px solid #c6cbd1;
            }}
            
            table tr:nth-child(2n) {{
                background-color: #f6f8fa;
            }}
            
            img {{
                max-width: 100%;
                box-sizing: border-box;
            }}
            
            .code-block {{
                background-color: #f6f8fa;
                border-radius: 3px;
                padding: 1em;
                margin: 1em 0;
            }}
            
            hr {{
                height: 0.25em;
                padding: 0;
                margin: 24px 0;
                background-color: #e1e4e8;
                border: 0;
            }}
            
            /* 数式のスタイル */
            .math {{
                font-size: 100%;
            }}
            
            /* 脚注のスタイル */
            .footnote {{
                font-size: 0.85em;
                vertical-align: super;
            }}
            
            /* ユーザー指定のスタイル */
            {custom_css}
        </style>
        <script type="text/x-mathjax-config">
            MathJax.Hub.Config({{
                tex2jax: {{
                    inlineMath: [['$','$'], ['\\\\(','\\\\)']],
                    processEscapes: true
                }}
            }});
        </script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML"></script>
    </head>
    <body>
        {str(soup)}
    </body>
    </html>
    """
    
    # 一時的なHTMLファイルを作成
    with tempfile.NamedTemporaryFile(suffix='.html', delete=False, mode='w', encoding='utf-8') as f:
        temp_html_file = f.name
        f.write(complete_html)
    
    try:
        # wkhtmltopdfのオプション
        options = {
            'page-size': 'A4',
            'margin-top': '10mm',
            'margin-right': '10mm',
            'margin-bottom': '10mm',
            'margin-left': '10mm',
            'encoding': 'UTF-8',
            'no-outline': None,
            'quiet': '',
            'enable-local-file-access': '',
        }
        
        # HTMLをPDFに変換
        pdfkit.from_file(temp_html_file, output_file, options=options, configuration=config)
        
        print(f"Conversion completed: {output_file}")
        return output_file
    finally:
        # 一時ファイルを削除
        if os.path.exists(temp_html_file):
            os.unlink(temp_html_file)


def main():
    parser = argparse.ArgumentParser(description='Markdown to PDF converter')
    parser.add_argument('input', help='Input markdown file')
    parser.add_argument('-o', '--output', help='Output PDF file')
    parser.add_argument('-s', '--style', help='Additional CSS file for styling')
    args = parser.parse_args()
    
    try:
        output_file = convert_md_to_pdf(args.input, args.output, args.style)
        print(f"Successfully created PDF: {output_file}")
    except Exception as e:
        print(f"Error: {e}")
        return 1
    
    return 0

if __name__ == '__main__':
    exit(main())
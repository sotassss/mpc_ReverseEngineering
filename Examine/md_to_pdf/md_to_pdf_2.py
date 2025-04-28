import os
from md2pdf.core import md2pdf

def convert_markdown_to_pdf(input_file, output_file=None):
    """
    Markdownファイルを読み込み、PDFファイルに変換する
    
    Parameters:
        input_file (str): 入力となるMarkdownファイルのパス
        output_file (str, optional): 出力PDFファイルのパス。指定がない場合は入力ファイルの拡張子をpdfに変更したものを使用
    
    Returns:
        str: 出力されたPDFファイルのパス
    """
    # 出力ファイル名が指定されていない場合は、入力ファイル名の拡張子をpdfに変更
    if output_file is None:
        base_name = os.path.splitext(input_file)[0]
        output_file = f"{base_name}.pdf"
    
    try:
        # MarkdownをPDFに変換
        md2pdf(
            output_file,
            md_file_path=input_file,
            css_file_path=None,  # CSSファイルのパスを指定可能
            base_url=None
        )
        
        print(f"PDFが正常に生成されました: {output_file}")
        return output_file
    except Exception as e:
        print(f"PDF生成中にエラーが発生しました: {e}")
        return None

if __name__ == '__main__':import os
import markdown
import pdfkit
from mdx_gfm import GithubFlavoredMarkdownExtension
from bs4 import BeautifulSoup
import re
import logging

# ロギングの設定
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def convert_markdown_to_pdf(input_file, output_file=None, css_file=None):
    """
    Markdownファイルを高品質なPDFファイルに変換する
    
    Parameters:
        input_file (str): 入力となるMarkdownファイルのパス
        output_file (str, optional): 出力PDFファイルのパス。指定がない場合は入力ファイルの拡張子をpdfに変更したものを使用
        css_file (str, optional): スタイル定義用のCSSファイルのパス
    
    Returns:
        str: 出力されたPDFファイルのパス
    """
    # 出力ファイル名が指定されていない場合は、入力ファイル名の拡張子をpdfに変更
    if output_file is None:
        base_name = os.path.splitext(input_file)[0]
        output_file = f"{base_name}.pdf"
    
    # Markdownファイルを読み込む
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            markdown_text = f.read()
    except FileNotFoundError:
        logger.error(f"エラー: ファイル '{input_file}' が見つかりません。")
        return None
    except Exception as e:
        logger.error(f"ファイル読み込み中にエラーが発生しました: {e}")
        return None
    
    # GitHub Flavored Markdownをサポートする拡張機能を含めてMarkdownをHTMLに変換
    html_text = markdown.markdown(
        markdown_text, 
        extensions=[
            'markdown.extensions.extra',  # テーブル、脚注などをサポート
            'markdown.extensions.codehilite',  # コードのシンタックスハイライト
            'markdown.extensions.toc',  # 目次の生成
            GithubFlavoredMarkdownExtension(),  # GitHub風Markdownをサポート
            'markdown.extensions.nl2br',  # 改行を<br>に変換
            'markdown.extensions.sane_lists',  # リストの生成を改善
        ]
    )
    
    # mermaid図の処理
    html_text = process_mermaid(html_text)
    
    # HTMLを整形し、スタイルを適用する
    styled_html = create_styled_html(html_text, css_file)
    
    # 一時HTMLファイル（必要な場合）
    temp_html = f"{os.path.splitext(input_file)[0]}_temp.html"
    
    try:
        # HTMLを一時ファイルに保存
        with open(temp_html, 'w', encoding='utf-8') as f:
            f.write(styled_html)
        
        # pdfkitの設定
        options = {
            'page-size': 'A4',
            'margin-top': '20mm',
            'margin-right': '20mm',
            'margin-bottom': '20mm',
            'margin-left': '20mm',
            'encoding': 'UTF-8',
            'quiet': '',
            'enable-local-file-access': '',  # ローカルファイルアクセスを有効に
        }
        
        # HTMLからPDFに変換
        pdfkit.from_file(temp_html, output_file, options=options)
        logger.info(f"PDFが正常に生成されました: {output_file}")
        
        # 一時HTMLファイルを削除
        os.remove(temp_html)
        
        return output_file
    
    except Exception as e:
        logger.error(f"PDF生成中にエラーが発生しました: {e}")
        if os.path.exists(temp_html):
            os.remove(temp_html)
        return None

def process_mermaid(html_text):
    """
    Mermaid図の処理を行う関数
    HTMLテキスト内の<pre><code class="mermaid">...</code></pre>を探し、
    これをMermaid.jsで描画できる形式に変換
    
    Parameters:
        html_text (str): 処理対象のHTML文字列
    
    Returns:
        str: Mermaid図を処理したHTML文字列
    """
    soup = BeautifulSoup(html_text, 'html.parser')
    
    # mermaidコードブロックを検索
    mermaid_blocks = soup.find_all('code', class_='mermaid')
    
    for block in mermaid_blocks:
        # mermaid図のdiv要素を作成
        mermaid_div = soup.new_tag('div', attrs={'class': 'mermaid'})
        mermaid_div.string = block.string
        
        # 親のpre要素を置き換え
        if block.parent.name == 'pre':
            block.parent.replace_with(mermaid_div)
        else:
            block.replace_with(mermaid_div)
    
    return str(soup)

def create_styled_html(html_content, css_file=None):
    """
    HTMLコンテンツにスタイルを適用し、完全なHTMLドキュメントを作成する
    
    Parameters:
        html_content (str): スタイルを適用するHTML文字列
        css_file (str, optional): 外部CSSファイルのパス
    
    Returns:
        str: スタイルが適用された完全なHTML文書
    """
    # デフォルトのCSS
    default_css = """
    body {
        font-family: Arial, Helvetica, sans-serif;
        line-height: 1.6;
        max-width: 900px;
        margin: 0 auto;
        padding: 20px;
        color: #333;
    }
    h1, h2, h3, h4, h5, h6 {
        color: #0066cc;
        margin-top: 24px;
        margin-bottom: 16px;
        font-weight: 600;
    }
    h1 {
        font-size: 2em;
        border-bottom: 1px solid #eaecef;
        padding-bottom: 0.3em;
    }
    h2 {
        font-size: 1.5em;
        border-bottom: 1px solid #eaecef;
        padding-bottom: 0.3em;
    }
    a {
        color: #0366d6;
        text-decoration: none;
    }
    a:hover {
        text-decoration: underline;
    }
    pre {
        background-color: #f6f8fa;
        border-radius: 3px;
        padding: 16px;
        overflow: auto;
    }
    code {
        font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
        font-size: 85%;
        background-color: rgba(27, 31, 35, 0.05);
        border-radius: 3px;
        padding: 0.2em 0.4em;
    }
    pre code {
        background-color: transparent;
        padding: 0;
    }
    table {
        border-collapse: collapse;
        width: 100%;
        margin: 16px 0;
    }
    table, th, td {
        border: 1px solid #dfe2e5;
    }
    th, td {
        padding: 8px 16px;
        text-align: left;
    }
    th {
        background-color: #f6f8fa;
    }
    tr:nth-child(even) {
        background-color: #f6f8fa;
    }
    img {
        max-width: 100%;
    }
    blockquote {
        padding: 0 1em;
        color: #6a737d;
        border-left: 0.25em solid #dfe2e5;
        margin: 0;
    }
    hr {
        height: 0.25em;
        padding: 0;
        margin: 24px 0;
        background-color: #e1e4e8;
        border: 0;
    }
    .mermaid {
        text-align: center;
    }
    """
    
    # 外部CSSファイルが指定されている場合、そのファイルの内容を読み込む
    custom_css = ""
    if css_file and os.path.exists(css_file):
        try:
            with open(css_file, 'r', encoding='utf-8') as f:
                custom_css = f.read()
        except Exception as e:
            logger.warning(f"CSSファイルの読み込み中にエラーが発生しました: {e}")
    
    # Mermaid.jsのサポートを追加
    mermaid_support = """
    <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
    <script>
        mermaid.initialize({
            startOnLoad: true,
            theme: 'default'
        });
    </script>
    """
    
    # HTMLテンプレート
    html_template = f"""<!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Markdown to PDF</title>
        <style>
            {default_css}
            {custom_css}
        </style>
        {mermaid_support}
    </head>
    <body>
        {html_content}
    </body>
    </html>
    """
    
    return html_template

def ensure_dependencies():
    """
    必要な依存関係が満たされているか確認し、満たされていない場合は
    インストール方法を表示する関数
    
    Returns:
        bool: 依存関係が満たされている場合はTrue、そうでない場合はFalse
    """
    try:
        import pdfkit
        import markdown
        from mdx_gfm import GithubFlavoredMarkdownExtension
        from bs4 import BeautifulSoup
        
        # wkhtmltopdfのパスを確認
        path_wkhtmltopdf = pdfkit.configuration().wkhtmltopdf
        if not os.path.exists(path_wkhtmltopdf):
            logger.warning(f"wkhtmltopdfが見つかりません: {path_wkhtmltopdf}")
            print_installation_guide()
            return False
            
        return True
        
    except ImportError as e:
        logger.error(f"必要なライブラリがインストールされていません: {e}")
        print_installation_guide()
        return False

def print_installation_guide():
    """依存関係のインストール方法を表示する関数"""
    print("\n必要な依存関係をインストールするには:")
    print("1. 以下のコマンドを実行してPythonライブラリをインストールします:")
    print("   pip install markdown py-gfm pdfkit beautifulsoup4")
    print("\n2. wkhtmltopdfをインストールします:")
    print("   - Windows: https://wkhtmltopdf.org/downloads.html からインストーラをダウンロード")
    print("   - macOS: 'brew install wkhtmltopdf'")
    print("   - Ubuntu/Debian: 'sudo apt-get install wkhtmltopdf'")
    print("   - CentOS/RHEL: 'sudo yum install wkhtmltopdf'")
    print("\nすべての依存関係をインストールした後に、このスクリプトを再実行してください。")

if __name__ == '__main__':
    # 依存関係を確認
    if not ensure_dependencies():
        exit(1)
    
    # ここで直接入力ファイルと出力ファイルを指定
    input_file_path = "sample.md"  # ここに変換したいMDファイルのパスを指定
    output_file_path = None  # Noneにすると自動的に同名のPDFファイルを作成
    css_file_path = None  # カスタムCSSファイルがある場合は指定
    
    convert_markdown_to_pdf(input_file_path, output_file_path, css_file_path)
    # ここで直接入力ファイルと出力ファイルを指定
    input_file_path = "sample.md"  # ここに変換したいMDファイルのパスを指定
    output_file_path = None  # Noneにすると自動的に同名のPDFファイルを作成
    
    convert_markdown_to_pdf(input_file_path, output_file_path)
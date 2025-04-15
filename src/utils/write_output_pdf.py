import os
from datetime import datetime
from markdown import markdown
from weasyprint import HTML

def write_output_pdf(output):
    # ファイル名（日時付き）を生成
    current_time = datetime.now().strftime("%Y_%m_%d_%H%M")
    filename_base = f"Document_{current_time}"

    # Markdown内容構築
    md_content = f"# {output.title}\n\n"
    for i, doc in enumerate(output.documents, 1):
        md_content += f"## セクション {i}\n\n{doc}\n\n"

    # PDF保存先
    pdf_directory = "./output_pdf/"
    os.makedirs(pdf_directory, exist_ok=True)
    pdf_path = os.path.join(pdf_directory, f"{filename_base}.pdf")

    # Markdown → HTML → PDF 変換
    html_content = markdown(md_content)
    HTML(string=html_content).write_pdf(pdf_path)

    print(f"PDFとして保存しました: {pdf_path}")

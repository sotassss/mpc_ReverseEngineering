from RAG_utils.pdf_loader import pdf_loader
from RAG_utils.RAG_self_route import answer_with_self_route

# 実行
if __name__ == "__main__":
    pdf_path = "sample.pdf"
    query = "内容を要約してください"
    
    # PDFの読み込み・埋め込み
    db,full_document_content=pdf_loader(pdf_path)
    
    # セルフルーティングで回答生成
    result = answer_with_self_route(query, db, full_document_content)
    
    # 結果を表示
    print(f"質問: {result['query']}")
    print(f"回答方法: {result['method']}")
    print(f"理由: {result['reasoning']}")
    print(f"回答: {result['answer'].content}")

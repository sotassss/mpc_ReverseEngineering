import re
import tiktoken

encoding = tiktoken.encoding_for_model("gpt-4o-mini")

# トークン数をカウントする関数
def count_tokens(text):
    return len(encoding.encode(text))

def intelligent_split(text, max_tokens):
    """
    テキストを文脈を考慮して適切に分割する関数
    
    Args:
        text (str): 分割する元のテキスト
        max_tokens (int): 1チャンクあたりの目安となる最大トークン数
    
    Returns:
        list: 分割されたテキストのリスト
    """
    
    # 全体のトークン数を確認し、超えていなければ分割不要
    if count_tokens(text) <= max_tokens:
        return [text]
    
    # パターン定義
    class_pattern = re.compile(r'^\s*class\s+\w+[\s\S]*?(?=^\s*class\s+\w+|\Z)', re.MULTILINE)
    function_pattern = re.compile(r'^\s*def\s+\w+[\s\S]*?(?=^\s*def\s+\w+|^\s*class\s+\w+|\Z)', re.MULTILINE)
    paragraph_pattern = re.compile(r'.*?(?:\n\s*\n|$)', re.DOTALL)
    
    # テキストを論理的な単位（クラス、関数、段落など）に分解
    segments = []
    
    # 1. まずクラス単位で分割
    class_matches = list(class_pattern.finditer(text))
    if class_matches:
        last_end = 0
        for match in class_matches:
            # クラス定義前のテキストがあれば追加
            if match.start() > last_end:
                pre_text = text[last_end:match.start()]
                if pre_text.strip():
                    segments.append(pre_text)
            # クラス本体を追加
            segments.append(match.group())
            last_end = match.end()
        
        # 最後のクラス以降のテキストがあれば追加
        if last_end < len(text):
            remaining_text = text[last_end:]
            if remaining_text.strip():
                segments.append(remaining_text)
    else:
        # クラスがない場合は関数単位で分割
        function_matches = list(function_pattern.finditer(text))
        if function_matches:
            last_end = 0
            for match in function_matches:
                # 関数定義前のテキストがあれば追加
                if match.start() > last_end:
                    pre_text = text[last_end:match.start()]
                    if pre_text.strip():
                        segments.append(pre_text)
                # 関数本体を追加
                segments.append(match.group())
                last_end = match.end()
            
            # 最後の関数以降のテキストがあれば追加
            if last_end < len(text):
                remaining_text = text[last_end:]
                if remaining_text.strip():
                    segments.append(remaining_text)
        else:
            # クラスも関数もない場合は段落単位で分割
            paragraphs = re.split(r'\n\s*\n', text)
            segments = [p for p in paragraphs if p.strip()]
    
    # 2. セグメントがトークン制限を超える場合は、さらに細かく分割
    refined_segments = []
    for segment in segments:
        segment_tokens = count_tokens(segment)
        
        if segment_tokens <= max_tokens:
            refined_segments.append(segment)
        else:
            # 大きいセグメントをさらに分割
            if "class " in segment[:20] or "def " in segment[:20]:
                # クラスや関数の場合は、行単位で分割
                lines = segment.split('\n')
                # 最初の行（宣言部分）を取得
                declaration = lines[0]
                refined_segments.append(declaration)
                
                # 残りの行をグループ化
                current_group = []
                current_tokens = count_tokens(declaration)
                
                for line in lines[1:]:
                    line_tokens = count_tokens(line)
                    
                    # この行を追加するとトークン制限を超える場合
                    if current_tokens + line_tokens > max_tokens and current_group:
                        refined_segments.append('\n'.join(current_group))
                        current_group = []
                        current_tokens = 0
                    
                    current_group.append(line)
                    current_tokens += line_tokens
                
                # 残りの行をチャンクに追加
                if current_group:
                    refined_segments.append('\n'.join(current_group))
            else:
                # 段落やその他のテキストの場合は、文単位で分割
                sentences = split_into_sentences(segment)
                current_group = []
                current_tokens = 0
                
                for sentence in sentences:
                    sentence_tokens = count_tokens(sentence)
                    
                    # この文を追加するとトークン制限を超える場合
                    if current_tokens + sentence_tokens > max_tokens and current_group:
                        refined_segments.append(' '.join(current_group))
                        current_group = []
                        current_tokens = 0
                    
                    current_group.append(sentence)
                    current_tokens += sentence_tokens
                
                # 残りの文をチャンクに追加
                if current_group:
                    refined_segments.append(' '.join(current_group))
    
    # 3. 最終的なチャンクを作成（トークン制限ギリギリまで結合）
    final_chunks = []
    current_chunk = []
    current_tokens = 0
    
    for segment in refined_segments:
        segment_tokens = count_tokens(segment)
        
        # 単一セグメントがトークン制限を超える場合（通常ここには来ないはず）
        if segment_tokens > max_tokens:
            if current_chunk:
                final_chunks.append('\n\n'.join(current_chunk))
                current_chunk = []
                current_tokens = 0
            final_chunks.append(segment)
            continue
        
        # このセグメントを追加するとトークン制限を超える場合
        if current_tokens + segment_tokens > max_tokens:
            final_chunks.append('\n\n'.join(current_chunk))
            current_chunk = [segment]
            current_tokens = segment_tokens
        else:
            current_chunk.append(segment)
            current_tokens += segment_tokens
    
    # 残りのチャンクを追加
    if current_chunk:
        final_chunks.append('\n\n'.join(current_chunk))
    
    return final_chunks

def split_into_sentences(text):
    """テキストを文単位で分割"""
    # 日本語と英語の文末パターンを考慮
    sentence_pattern = re.compile(r'[^。.!?]+[。.!?]|[^。.!?]+$')
    sentences = sentence_pattern.findall(text)
    return [s for s in sentences if s.strip()]
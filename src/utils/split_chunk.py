# テキストを文脈を考慮して分割する

import re
import tiktoken
import os

encoding=tiktoken.encoding_for_model("gpt-4o-mini")
# トークン数をカウントする関数
def count_tokens(text):
    return len(encoding.encode(text))  # tiktokenのエンコード方法に基づくトークン数計算

def intelligent_split(text, max_tokens):
    """
    テキストを文脈を考慮して適切に分割する関数
    
    Args:
        text (str): 分割する元のテキスト
        max_tokens (int): 1チャンクあたりの目安となる最大トークン数
    
    Returns:
        list: 分割されたテキストのリスト
    """
    
    # クラス定義、関数定義、段落などの区切りを検出するパターン
    class_pattern = re.compile(r'^\s*class\s+\w+[\s\S]*?(?=^\s*class\s+\w+|\Z)', re.MULTILINE)
    function_pattern = re.compile(r'^\s*def\s+\w+[\s\S]*?(?=^\s*def\s+\w+|^\s*class\s+\w+|\Z)', re.MULTILINE)
    paragraph_pattern = re.compile(r'.*?(?:\n\s*\n|$)', re.DOTALL)

    # まず全体のトークン数を確認し、超えていれば分割
    if count_tokens(text) <= max_tokens:
        return [text]
    
    # 1. クラス単位で分割を試みる
    class_matches = list(class_pattern.finditer(text))
    if class_matches:
        chunks = []
        last_end = 0
        
        for match in class_matches:
            # クラス定義前のテキストがあれば追加
            if match.start() > last_end:
                pre_text = text[last_end:match.start()]
                if pre_text.strip():
                    chunks.append(pre_text)
            
            # クラス本体を追加
            class_text = match.group()
            if count_tokens(class_text) > max_tokens * 1.5:
                # クラスが大きすぎる場合は関数単位でさらに分割
                class_lines = class_text.split('\n')
                class_declaration = class_lines[0]
                class_body = '\n'.join(class_lines[1:])
                method_chunks = split_by_functions(class_body)
                
                if method_chunks:
                    method_chunks[0] = class_declaration + '\n' + method_chunks[0]
                    chunks.extend(method_chunks)
                else:
                    chunks.append(class_text)
            else:
                chunks.append(class_text)
            
            last_end = match.end()
        
        # 最後のクラス以降のテキストがあれば追加
        if last_end < len(text):
            remaining_text = text[last_end:]
            if remaining_text.strip():
                chunks.append(remaining_text)
    else:
        # 2. クラスがない場合は関数単位で分割
        chunks = split_by_functions(text)
        
        # 3. 関数もない場合は段落単位で分割
        if len(chunks) <= 1:
            chunks = split_by_paragraphs(text, max_tokens)
    
    # 4. チャンクのサイズをチェックし、大きすぎる場合はさらに分割（段落または文単位）
    final_chunks = []
    for chunk in chunks:
        if count_tokens(chunk) > max_tokens * 1.5:
            # 大きいチャンクをさらに分割（段落単位）
            sub_chunks = split_by_paragraphs(chunk, max_tokens)
            final_chunks.extend(sub_chunks)
        else:
            final_chunks.append(chunk)
    
    # 最終的に文単位で分割
    if final_chunks:
        smallest_chunks = []
        for chunk in final_chunks:
            sub_chunks = split_into_sentences(chunk)
            smallest_chunks.extend(sub_chunks)
        return smallest_chunks
    
    return final_chunks


def split_by_functions(text):
    """関数単位でテキストを分割"""
    function_pattern = re.compile(r'^\s*def\s+\w+[\s\S]*?(?=^\s*def\s+\w+|\Z)', re.MULTILINE)
    
    function_matches = list(function_pattern.finditer(text))
    if not function_matches:
        return [text]
    
    chunks = []
    last_end = 0
    
    for match in function_matches:
        # 関数定義前のテキストがあれば追加
        if match.start() > last_end:
            pre_text = text[last_end:match.start()]
            if pre_text.strip():
                chunks.append(pre_text)
        
        # 関数本体を追加
        chunks.append(match.group())
        last_end = match.end()
    
    # 最後の関数以降のテキストがあれば追加
    if last_end < len(text):
        remaining_text = text[last_end:]
        if remaining_text.strip():
            chunks.append(remaining_text)
    
    return chunks

def split_by_paragraphs(text, max_tokens):
    """段落単位でテキストを分割し、トークン数の制限を考慮"""
    paragraphs = re.split(r'\n\s*\n', text)
    
    chunks = []
    current_chunk = []
    current_token_count = 0
    
    for paragraph in paragraphs:
        paragraph = paragraph.strip()
        if not paragraph:
            continue
        
        # 段落のトークン数を概算（簡易的に単語数で計算）
        paragraph_tokens = count_tokens(paragraph)
        
        # この段落を追加するとトークン制限を超える場合
        if current_token_count + paragraph_tokens > max_tokens and current_chunk:
            chunks.append('\n\n'.join(current_chunk))
            current_chunk = []
            current_token_count = 0
        
        # 段落自体が制限を超える場合は文単位で分割
        if paragraph_tokens > max_tokens:
            sentences = split_into_sentences(paragraph)
            sentence_chunks = []
            
            temp_chunk = []
            temp_token_count = 0
            
            for sentence in sentences:
                sentence_tokens = len(sentence.split())
                
                if temp_token_count + sentence_tokens > max_tokens and temp_chunk:
                    sentence_chunks.append(' '.join(temp_chunk))
                    temp_chunk = []
                    temp_token_count = 0
                
                temp_chunk.append(sentence)
                temp_token_count += sentence_tokens
            
            if temp_chunk:
                sentence_chunks.append(' '.join(temp_chunk))
            
            for sc in sentence_chunks:
                if current_token_count + len(sc.split()) > max_tokens and current_chunk:
                    chunks.append('\n\n'.join(current_chunk))
                    current_chunk = []
                    current_token_count = 0
                
                current_chunk.append(sc)
                current_token_count += len(sc.split())
        else:
            current_chunk.append(paragraph)
            current_token_count += paragraph_tokens
    
    # 残りのチャンクを追加
    if current_chunk:
        chunks.append('\n\n'.join(current_chunk))
    
    return chunks

def split_into_sentences(text):
    """テキストを文単位で分割"""
    # 日本語と英語の文末パターンを考慮
    sentence_pattern = re.compile(r'[^。.!?]+[。.!?]|[^。.!?]+$')
    sentences = sentence_pattern.findall(text)
    return sentences
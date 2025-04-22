import os
import chardet
import fnmatch
from charset_normalizer import from_bytes
from src.utils.split_chunk import intelligent_split
from src.utils.config import load_config

# 設定を読み込む
config = load_config()

def is_sensitive_file(file_path: str, config: dict) -> bool:
    """
    ファイルがセンシティブかどうかを判定する
    Args:
        file_path: ファイルパス
        config: 設定パラメータ
    Returns:
        bool: センシティブな場合はTrue
    """
    # ファイル名パターンのチェック
    file_name = os.path.basename(file_path)

    patterns = config.get("sensitive_patterns", [])
    # print(f"チェック対象パターン数: {len(patterns)}")
    
    for pattern in patterns:
        if fnmatch.fnmatch(file_name.lower(), pattern.lower()):
            # print(f"※ センシティブファイル検出 : {file_name} → {pattern}")
            return True
    return False

def guess_encoding(file_path):
    """
    指定されたファイルのエンコーディングを推測する関数
    """
    with open(file_path, 'rb') as f:
        rawdata = f.read()
    # # 手法1：chardet を使ってエンコーディングを推測
    # result = chardet.detect(rawdata)
    # encoding = result['encoding']

    # 手法2：charset_normalizer を使ってエンコーディングを推測
    result = from_bytes(rawdata).best()
    encoding = result.encoding if result else None

    return encoding, rawdata

def extract_text(file_path):
    """
    指定されたファイルからテキストを抽出する関数
    """
    encoding, rawdata = guess_encoding(file_path)
    if not encoding:
        print(f"{file_path}：エンコーディングを検出できませんでした。")
        return None
    try:
        text = rawdata.decode(encoding)
    except UnicodeDecodeError as e:
        print(f"デコード中にエラーが発生しました: {e}")
        return None
    max_tokens = config.get("llm", {}).get("split_max_tokens",30000)
    # return text
    chunks = intelligent_split(text,max_tokens=max_tokens)
    # print(f"ファイル: {file_path}、チャンク数: {len(chunks)}")
    return chunks
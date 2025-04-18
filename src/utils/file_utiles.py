import os
import chardet
import fnmatch

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

    # デバッグ用
    # hardcoded_patterns = ["*.pfx", "*.p12", "config*.yml", "*.env"]
    # for pattern in hardcoded_patterns:
    #     if fnmatch.fnmatch(file_name, pattern):
    #         print(f"センシティブファイル検出 (ハードコード): {file_name} → {pattern}")
    #         return True
        
    patterns = config.get("sensitive_patterns", [])
    # print(f"チェック対象パターン数: {len(patterns)}")
    
    for pattern in patterns:
        # print(f"パターンチェック: '{file_name.lower()}' と '{pattern.lower()}'")
        if fnmatch.fnmatch(file_name.lower(), pattern.lower()):
            print(f"※ センシティブファイル検出 : {file_name} → {pattern}")
            return True
    return False

def guess_encoding(file_path):
    """
    指定されたファイルのエンコーディングを推測する関数
    """
    with open(file_path, 'rb') as f:
        rawdata = f.read()
    # chardet を使ってエンコーディングを推測
    result = chardet.detect(rawdata)
    encoding = result['encoding']
    return encoding, rawdata

def extract_text(file_path):
    """
    指定されたファイルからテキストを抽出する関数
    """
    encoding, rawdata = guess_encoding(file_path)
    if not encoding:
        print("エンコーディングを検出できませんでした。")
        return None
    try:
        text = rawdata.decode(encoding)
    except UnicodeDecodeError as e:
        print(f"デコード中にエラーが発生しました: {e}")
        return None
    return text
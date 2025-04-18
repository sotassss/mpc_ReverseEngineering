import os
import glob

def collect_all_files(folder_path: str) -> list[str]:
    # フォルダ配下のすべてのファイルを再帰的に収集
    pattern = os.path.join(folder_path, "**", "*")
    return [f for f in glob.glob(pattern, recursive=True) if os.path.isfile(f)]
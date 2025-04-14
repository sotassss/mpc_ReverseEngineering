# レガシーシステム リバースエンジニアリング AI

このプロジェクトは、レガシーシステムのソースコードを解析し、要件定義書や仕様書を自動生成するAIエージェントシステムです。COBOLなどの古い言語で書かれたシステムだけでなく、様々なレガシーシステムに対応しています。

## 機能

- **個別ファイル解析**: 各ソースファイルの詳細な解析と要約
- **動的グループ化**: 関連するファイルを自動的にグループ化
- **統合ドキュメント生成**: グループ単位の概要と詳細な仕様書の生成
- **整合性チェック**: 生成されたドキュメント間の整合性検証

## システム構成

このシステムはLangGraphを使用したマルチエージェントシステムとして実装されています。

1. **ScriptAnalysisNode**: 個別のソースファイルを解析
2. **DynamicGroupingNode**: ファイルの解析結果を基にグループ化
3. **DocumentGenerationNode**: グループごとのドキュメントセクションを生成
4. **ConsistencyCheckNode**: ドキュメント間の整合性をチェック
5. **OutlinedOrchestrationNode**: 全体のアウトラインを管理・生成

## 使用方法

1. リポジトリをクローン:
```
git clone [repository-url]
cd legacy-reverse-engineering-ai
```

2. 依存関係をインストール:
```
pip install -r requirements.txt
```

3. `.env.example`を`.env`にコピーし、必要な環境変数を設定:
```
cp .env.example .env
```

4. 解析したいレガシーコードを`data/sample_legacy_code`ディレクトリに配置

5. メインプログラムを実行:
```
python src/main.py
```

6. 生成されたドキュメントは`output`ディレクトリに保存されます

## 必要環境

- Python 3.10+
- OpenAI API キー

## ステート図
<img width="564" alt="Image" src="https://github.com/user-attachments/assets/5951dc9b-32cc-4215-be94-b7afb28217a3" />

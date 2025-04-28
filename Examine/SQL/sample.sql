-- データベース作成（もし存在しなければ作成）
CREATE DATABASE IF NOT EXISTS sample_db;

-- データベース選択
USE sample_db;

-- テーブル作成
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- データ挿入
INSERT INTO users (name, email)
VALUES ('田中 太郎', 'taro.tanaka@example.com');

INSERT INTO users (name, email)
VALUES ('鈴木 花子', 'hanako.suzuki@example.com');

-- 全データ取得
SELECT * FROM users;

-- 名前に「太郎」が含まれる人を検索
SELECT * FROM users
WHERE name LIKE '%太郎%';

-- メールアドレスを更新
UPDATE users
SET email = 'updated.taro@example.com'
WHERE name = '田中 太郎';

-- 特定ユーザーを削除
DELETE FROM users
WHERE name = '鈴木 花子';

-- テーブル削除（必要なら）
-- DROP TABLE users;

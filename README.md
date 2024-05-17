# 🏭d2gen
- d2gen = **`d`** ummy **`d`** ata **`gen`** erater  
- ダミーデータを作成する

# ✨主な機能
- 標準記法の符号表ファイル or 符号表の情報のJSONデータからダミーデータを作成する
- 出力するレコード数を指定できる
- 作成される符号はコードリストの一覧からランダムにピックアップする
- コードリストの指定がなかった場合はランダムな文字列を設定する
  - デフォルトは `A-Z` + `a-z` + `0-9` の組み合わせ
  - 使用する文字列は指定可能

## 標準記法の符号表から
- 標準記法の内容をよしなに解析してデータを作成する

## JSONデータから
- 事項の情報と符号の情報を基にデータを作成する


# 🚀Getting Started
- miripoからダウンロードできる令和２年の国勢調査の符号表からダミーデータを作成する例です
- 符号表は事前にtestディレクトリにダウンロードしているものを使っています

```shell
git clone git@github.com:tsutomu-nagano/d2gen.git
cd d2gen
src=test/【オンサイト用】（標準記法）令和2年国調個別データCP_2020_RCD_Kobetsu-kk_B(基本集計).xlsx
dest=test.csv
docker run --rm -v $PWD:/home/d2gen d2gen --src "${src}" --dest "${dest}" --rec 5
```

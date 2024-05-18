# 🏭d2gen

- d2gen = **`d`** ummy **`d`** ata **`gen`** erater  
- ダミーデータを作成する

## ✨主な機能

- 標準記法の符号表ファイル　or 符号表の情報のJSONファイル からダミーデータを作成する
- 出力するレコード数を指定できる
- 作成される符号はコードリストの一覧からランダムにピックアップする
- コードリストの指定がなかった場合はランダムな文字列を設定する
  - デフォルトは `A-Z` + `a-z` + `0-9` の組み合わせ
  - 使用する文字列は指定可能
- 符号が `数値--数値` もしくは `数値～数値` の場合は、数値の範囲内でランダムな数値をピックアップする
  - 数値は `0-9.` の組み合わせだったらOKとしている 

<!-- ### 標準記法の符号表から

- 標準記法の内容をよしなに解析してデータを作成する -->

### JSONデータのスキーマ

- 以下はタブ区切りの可変長を出力する場合の定義

``` JSON
{
    "info":{
        "datatype": "variable",
        "delim": "\t"
    },
    "items":[
        {"id":1, "pos":1, "length":1, "code":["A","B","C"]},
        {"id":2, "pos":2, "length":2, "code":["01--99","VV"]},
        {"id":3, "pos":4, "length":3, "code":["1～999"]}
    ]
}
```

## 🚀Getting Started

- miripoからダウンロードできる令和２年の国勢調査の符号表からダミーデータを作成する例です
- 符号表は事前にtestディレクトリにダウンロードしているものを使っています

```sh
git clone git@github.com:tsutomu-nagano/d2gen.git
cd d2gen
docker build . -t d2gen:latest
src=test/【オンサイト用】（標準記法）令和2年国調個別データCP_2020_RCD_Kobetsu-kk_B(基本集計).xlsx
dest=test.csv
docker run --rm -v $PWD:/home/d2gen d2gen:latest --src "${src}" --dest "${dest}" --rec 5
```

### パラメータの説明

| パラメータ名 | 概要 |　必須 |　省略した場合の既定値 |
| ----- | ---------- | ---------- | ------ |
| --src | 処理対象の符号表ファイルのパス | 必須 | |
| --dest | 出力先のファイルのパス | 必須　|    |
| --rec | 出力レコード数 | | 100 |

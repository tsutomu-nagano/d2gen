docker build . -t d2gen:latest
# src="test/【オンサイト用】（標準記法）令和2年国調個別データCP_2020_RCD_Kobetsu-kk_B(基本集計).xlsx"
# dest="test.csv"

src="test/test.json"
dest="test1.csv"

docker run --rm -v $PWD:/home/d2gen d2gen --src "${src}" --dest "${dest}" --rec 50

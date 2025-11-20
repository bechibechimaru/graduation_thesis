import pyreadr

path = '/Users/karubeshougo/Uni/seminar/graduation_thesis/playground/dataset_g1_main (1).rds'

# RDSファイルを読み込み
result = pyreadr.read_r(path)

# データフレームを取得（RDSファイルには通常1つのオブジェクトが含まれている）
df = result[list(result.keys())[0]]

print("データ数:", len(df))
rm(list=ls())

# ワーキングディレクトリ
print(getwd() )

# パッケージ（追加機能）の追加
## エラーが出たら、install.packages("パッケージ名")でインストールすること。
library(ggplot2) # グラフ出力に使う。
theme_set(theme_bw())
library(texreg) ## 回帰表の出力
library(htmltools) ## HTMLで表をプレビュー
library(estimatr) ## ロバスト標準誤差を使った回帰分析（lm_robust）の実行
## データ読み込みのためのライブラリ
library(haven)
library(labelled)

## データセットのインポート ##
d <- read_sav("experiment_origin_data.sav") 

## 最後まで回答完了している人だけをデータに残す
table(d$Progress)
d <- subset(d, Progress == 100)
nrow(d) ## 1191人の有効回答あり

# フィルタリング後のデータをCSV形式で出力
write.csv(d, file = 'valid_data.csv', row.names = FALSE)

## 新しいデータセットを作成
## idで個人を特定
dn <- data.frame(id = d$ResponseId) #回答者ID
dn$gvtresp_1 <- d$gvtresp_1
dn$gvtresp_2 <- d$gvtresp_2

# 表をデータフレームに
t1 <- table(factor(round(dn$gvtresp_1), levels = 1:4))
df1 <- data.frame(値 = names(t1), 度数 = as.numeric(t1))

p1 <- ggplot(df1, aes(x = 値, y = 度数)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(title = "gvtresp_1の分布", x = "gvtresp_1", y = "度数") +
  theme_bw()
ggsave("gvtresp_1_hist.png", p1, width = 5, height = 4, dpi = 120)


# 表をデータフレームに
t2 <- table(factor(round(dn$gvtresp_2), levels = 1:4))
df2 <- data.frame(値 = names(t2), 度数 = as.numeric(t2))

p2 <- ggplot(df2, aes(x = 値, y = 度数)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(title = "gvtresp_2の分布", x = "gvtresp_2", y = "度数") +
  theme_bw()
ggsave("gvtresp_2_hist.png", p2, width = 5, height = 4, dpi = 120)
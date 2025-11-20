## H1

library(modelsummary)
library(ggplot2)
library(broom)



# ライブラリの読み込み
library(modelsummary)
library(dplyr) # データ加工に便利なため追加

# データの読み込みと前処理
current_dir <- setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis")
origin_data <- read.csv("Statics/くらしと科学技術に関する意識調査/sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", fileEncoding = "CP932", encoding = "UTF-8")

# --- データの前処理 ---

# 年齢(F2)を7つのカテゴリ（1〜7）に再分類
origin_data <- origin_data %>%
  mutate(
    age_category = case_when(
      F2 == 1 ~ 1,      # 10代
      F2 %in% c(2, 3) ~ 2,  # 20代
      F2 %in% c(4, 5) ~ 3,  # 30代
      F2 %in% c(6, 7) ~ 4,  # 40代
      F2 %in% c(8, 9) ~ 5,  # 50代
      F2 %in% c(10, 11) ~ 6, # 60代
      F2 %in% c(12, 13, 14) ~ 7, # 70代以上
      TRUE ~ NA_real_
    )
  )

# 学歴(F3)のNA処理
origin_data$F3[origin_data$F3 %in% c(6, 9)] <- NA

# 分析用データフレームの作成
analysis_data <- data.frame(
  # 従属変数
  is_important_q8_5 = ifelse(origin_data$Q8_5 %in% c(1, 2), 1, 0),
  is_important_q8_19 = ifelse(origin_data$Q8_19 %in% c(1, 2), 1, 0),
  
  # 独立変数
  trust_government = ifelse(origin_data$Q11_8 %in% c(1, 2), 1, 0),
  
  # 調整変数
  # factor()を使わず、数値のまま投入するのがポイント
  gender = origin_data$F1,
  age = origin_data$age_category, # 再分類した数値をそのまま使用
  education = origin_data$F3      # 数値をそのまま使用
) %>%
  na.omit() # 欠損値を持つ行を削除


# --- ロジスティック回帰分析 ---

# 単純モデル
model_1 <- glm(is_important_q8_5 ~ trust_government, data = analysis_data, family = "binomial")
model_2 <- glm(is_important_q8_19 ~ trust_government, data = analysis_data, family = "binomial")

# 多変量モデル（先行研究スタイル）
model_3 <- glm(is_important_q8_5 ~ trust_government + gender + age + education, data = analysis_data, family = "binomial")
model_4 <- glm(is_important_q8_19 ~ trust_government + gender + age + education, data = analysis_data, family = "binomial")


# --- `modelsummary`による表の作成 ---

# 表示名のカスタマイズ設定
coef_mapping <- c(
  "(Intercept)" = "切片 (Intercept)",
  "trust_government" = "政府への信頼",
  "gender" = "性別 (女性=2)",
  "age" = "年齢",
  "education" = "学歴"
)

# モデル適合度指標の表示名を定義
# 先行研究に合わせて Pseudo R^2 を追加
gof_mapping <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "サンプル数", 0,
  "r.squared.pseudo", "Pseudo R2", 4, # Pseudo R2 を追加
  "AIC", "AIC", 1
)

# 4つのモデルを結合
models_list <- list(
  "情報通信政策（単純）" = model_1,
  "情報通信政策（多変量）" = model_3,
  "科学技術イノベーション政策（単純）" = model_2,
  "科学技術イノベーション政策（多変量）" = model_4
)

# 整形されたサマリー表を作成
modelsummary(
  models_list,
  title = "表: 政府への信頼が各政策の重要度認識に与える影響（多変量解析）",
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001), # 星の基準を明記
  coef_map = coef_mapping,
  gof_map = gof_mapping,
  statistic = "({std.error})", # 標準誤差を括弧付きで表示
  notes = list("p値の凡例: * p < 0.05, ** p < 0.01, *** p < 0.001")
)
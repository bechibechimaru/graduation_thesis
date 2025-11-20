## H1

library(modelsummary)
library(ggplot2)
library(broom)

# データの読み込みと前処理
current_dir <- setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis")
origin_data <- read.csv("Statics/くらしと科学技術に関する意識調査/sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", fileEncoding = "CP932", encoding = "UTF-8")

analysis_data <- data.frame(
    # [従属変数] q8.5, q8.19 において、重要であると考えている
    is_important_q8_5 = ifelse(origin_data$Q8_5 == 1 | origin_data$Q8_5 == 2, 1, 0),
    is_important_q8_19 = ifelse(origin_data$Q8_19 == 1 | origin_data$Q8_19 == 2, 1, 0), 
    
    # [独立変数] 政府への信頼度
    trust_government = ifelse(origin_data$Q11_8 == 1 | origin_data$Q11_8 == 2, 1, 0)
)

# モデル1: 情報通信政策
model_1 <- glm(is_important_q8_5 ~ trust_government, data = analysis_data, family = "binomial")
# モデル2: 科学技術イノベーション政策
model_2 <- glm(is_important_q8_19 ~ trust_government, data = analysis_data, family = "binomial")


# --- 表示名のカスタマイズ設定 ---
# 係数の表示名を定義
coef_mapping <- c(
  "(Intercept)" = "切片 (Intercept)",
  "trust_government" = "政府への信頼"
)

# モデル適合度指標の表示名を定義
gof_mapping <- data.frame(
  raw = c("nobs", "AIC"),
  clean = c("サンプル数", "AIC"),
  fmt = c(0, 1)
)


## H1&H2
# 結合したモデルのリストを作成
models_combined <- list(
  "情報通信政策" = model_1,
  "科学技術イノベーション政策" = model_2
)

# 整形されたサマリー表を作成
modelsummary(
  models_combined,
  title = "表1: 政府への信頼が各政策の重要度認識に与える影響",
  stars = TRUE,
  coef_map = coef_mapping,
  gof_map = gof_mapping
)

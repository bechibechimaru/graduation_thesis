
library(modelsummary)

current_dir <- setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis")
origin_data <- read.csv("Statics/くらしと科学技術に関する意識調査/sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", fileEncoding = "CP932", encoding = "UTF-8")

analysis_data <- data.frame(
# [従属変数]q8.5, q8.19 において、重要であると考えている
    is_important_q8_19 = ifelse(origin_data$Q8_19 == 1 | origin_data$Q8_19 == 2, 1, 0), 
    
# [独立変数] 政府への信頼度
    trust_government = ifelse(origin_data$Q11_8 == 1 | origin_data$Q11_8 == 2, 1, 0)
)
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

# モデルを名前付きリストに格納
models <- list(
  "(2) 科学技術イノベーション政策" = model_2
)

# 整形されたサマリー表を作成
modelsummary(
  models,
  title = "表2: 政府への信頼が各政策の重要度認識に与える影響（単純モデル）",
  stars = TRUE,
  coef_map = coef_mapping,
  gof_map = gof_mapping
)


# 結果を図示する
print("政府への信頼が各政策の重要度認識に与える影響")
library(ggplot2)
library(broom)

tidy_model_2 <- tidy(model_2, conf.int = TRUE)

tidy_model_2$model <- "2:科学技術イノベーション政策\n(Q8.5)"

combined_models_filtered <- rbind(tidy_model_2)

# 係数プロットを作成する
revised_plot <- ggplot(combined_models_filtered, aes(x = estimate, y = model, color = model)) +
    geom_point(size = 4) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
    title = "政府への信頼が\n科学技術イノベーション政策の重要度認識に与える影響",
    subtitle = "ロジスティック回帰分析の係数プロット（95%信頼区間）",
    x = "係数（Estimate）",
    y = "モデル"
    ) +
    theme_classic() +
    theme(legend.position = "none")

ggsave("h2_graph.png", revised_plot, width = 10, height = 6)
print("グラフを保存しました")

library(dplyr)
library(scales)

bar_data <- analysis_data %>%
group_by()


# ------------------------------------------------------------
# 棒グラフの作成
# ------------------------------------------------------------
library(dplyr)      # データ集計にdplyrパッケージを使用
library(scales)     # y軸のラベルをパーセント表示にするために使用

# 1. 信頼度別に重要度の割合を計算
bar_data <- analysis_data %>%
  # trust_government (0, 1) でグループ化
  group_by(trust_government) %>%
  # グループごとに集計
  summarise(
    # is_important_q8_19 の平均値を計算 (これが割合になる)
    proportion_important = mean(is_important_q8_19, na.rm = TRUE)
  ) %>%
  # X軸のラベルを分かりやすくするための処理
  mutate(
    trust_label = factor(trust_government,
                         levels = c(0, 1),
                         labels = c("政府を信頼しない", "政府を信頼する"))
  )

# 2. 棒グラフを作成
bar_plot <- ggplot(bar_data, aes(x = trust_label, y = proportion_important, fill = trust_label)) +
  # 棒グラフを描画
  geom_col(width = 0.6) +
  # 棒の上に割合のテキストを表示
  geom_text(aes(label = percent(proportion_important, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  # y軸を0%から100%の範囲にし、パーセント表示に
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  # 各種ラベルを設定
  labs(
    title = "政府への信頼度と科学技術イノベーション政策の重要度認識",
    subtitle = "信頼度グループ別に「重要」と回答した人の割合",
    x = "政府への信頼",
    y = "重要だと回答した割合"
  ) +
  # シンプルなテーマを適用（フォント指定を一時的に削除）
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", # 凡例は不要なため非表示
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 3. グラフの表示と保存
print(bar_plot)
ggsave("h2_bar_chart.png", plot = bar_plot, width = 8, height = 6)
print("棒グラフを 'h2_bar_chart.png' として保存しました。")


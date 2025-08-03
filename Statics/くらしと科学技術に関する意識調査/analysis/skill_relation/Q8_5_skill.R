warnings()
current_path <- "/Users/karubeshougo/Uni/seminar/graduation_thesis/graduation_thess/statics/くらしと科学技術に関する意識調査/analysis/skill_relation"

setwd(current_path)

ra_data = read.csv("../../sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", fileEncoding = "Shift_JIS")

# Q1で1または2を回答している人を抽出
q1_positive_data <- ra_data[ra_data$Q1 %in% c(1, 2), ]
# Q1で4または5を回答している人を抽出
q1_negative_data <- ra_data[ra_data$Q1 %in% c(4, 5), ]

# [ITリテラシーが高い人]Q2で1を回答している人を抽出
q2_positive_data <- ra_data[ra_data$Q2 %in% c(1), ]
# [ITリテラシーが低い人]Q2で4を回答している人を抽出
q2_negative_data <- ra_data[ra_data$Q2 %in% c(2), ]

# [ITリテラシーが高い人]Q3で1を回答している人を抽出
q3_positive_data <- ra_data[ra_data$Q3 %in% c(1), ]
# [ITリテラシーが低い人]Q3で1以外を回答している人を抽出
q3_negative_data <- ra_data[ra_data$Q3 %in% c(2, 3, 4), ]

# [ITリテラシーが高い人]
it_positive_data <- q3_positive_data
# [ITリテラシーが低い人]
it_negative_data <- q3_negative_data

# Q8_5で1または2を回答している人を抽出
positive_filtered_data <- ra_data[ra_data$Q8_5 %in% c(1, 2), ]
# Q8_5で4または5を回答している人を抽出
negative_filtered_data <- ra_data[ra_data$Q8_5 %in% c(4, 5), ]

# 科学技術に関心がある人とITリテラシーの関係を分析
# ITリテラシーが高い人の中で科学技術に関心がある人の割合
high_it_interest <- sum(it_positive_data$Q8_5 %in% c(1, 2)) / nrow(it_positive_data) * 100
# ITリテラシーが低い人の中で科学技術に関心がある人の割合
low_it_interest <- sum(it_negative_data$Q8_5 %in% c(1, 2)) / nrow(it_negative_data) * 100

print("ITリテラシーと科学技術への関心の関係:")
print(paste("ITリテラシーが高い人の中で科学技術に関心がある人の割合:", round(high_it_interest, 2), "%"))
print(paste("ITリテラシーが低い人の中で科学技術に関心がある人の割合:", round(low_it_interest, 2), "%"))

# カイ二乗検定
contingency_table <- table(
  IT_Literacy = c(rep("High", nrow(it_positive_data)), rep("Low", nrow(it_negative_data))),
  Interest = c(it_positive_data$Q8_5 %in% c(1, 2), it_negative_data$Q8_5 %in% c(1, 2))
)
chi_square_test <- chisq.test(contingency_table)
print("\nカイ二乗検定の結果:")
print(chi_square_test)

# 相関分析と回帰分析のためのデータ準備
it_data <- data.frame(
  IT_Literacy = c(rep("High", nrow(it_positive_data)), rep("Low", nrow(it_negative_data))),
  Interest = c(it_positive_data$Q8_5 %in% c(1, 2), it_negative_data$Q8_5 %in% c(1, 2))
)

# 相関分析
correlation <- cor.test(
  as.numeric(it_data$IT_Literacy == "High"),
  as.numeric(it_data$Interest)
)
print("\n相関分析の結果:")
print(correlation)

# 回帰分析
lm_model <- lm(
  as.numeric(Interest) ~ as.numeric(IT_Literacy == "High"),
  data = it_data
)
print("\n回帰分析の結果:")
print(summary(lm_model))

# 視覚化
library(ggplot2)

# データフレームの作成
plot_data <- data.frame(
  IT_Literacy = c("High", "Low"),
  Interest_Ratio = c(high_it_interest, low_it_interest)
)

# 棒グラフの作成
p <- ggplot(plot_data, aes(x = IT_Literacy, y = Interest_Ratio, fill = IT_Literacy)) +
  geom_bar(stat = "identity") +
  labs(title = "ITリテラシーと科学技術への関心の関係",
       x = "ITリテラシー",
       y = "科学技術に関心がある人の割合 (%)",
       fill = "ITリテラシー") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "steelblue", "Low" = "lightblue"))

# グラフの保存
ggsave("../../sources/skill_relation/Q8_5_IT_literacy_science_interest.png", p, width = 10, height = 6)

# 散布図の作成
p2 <- ggplot(it_data, aes(x = as.numeric(IT_Literacy == "High"), y = as.numeric(Interest))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "ITリテラシーと科学技術への関心の相関",
       x = "ITリテラシー（High = 1, Low = 0）",
       y = "科学技術への関心（あり = 1, なし = 0）") +
  theme_minimal()

# グラフの保存
  ggsave("../../sources/skill_relation/Q8_5_IT_literacy_science_interest_correlation.png", p2, width = 10, height = 6)

# 詳細な分析結果をファイルに出力
sink("../../sources/skill_relation/Q8_5_IT_literacy_science_interest_analysis.txt")
cat("ITリテラシーと科学技術への関心の関係分析\n\n")
cat("1. 基本統計\n")
cat("ITリテラシーが高い人の総数:", nrow(it_positive_data), "\n")
cat("ITリテラシーが低い人の総数:", nrow(it_negative_data), "\n\n")
cat("2. 科学技術への関心\n")
cat("ITリテラシーが高い人の中で科学技術に関心がある人の割合:", round(high_it_interest, 2), "%\n")
cat("ITリテラシーが低い人の中で科学技術に関心がある人の割合:", round(low_it_interest, 2), "%\n\n")
cat("3. カイ二乗検定の結果\n")
print(chi_square_test)
cat("\n4. 相関分析の結果\n")
print(correlation)
cat("\n5. 回帰分析の結果\n")
print(summary(lm_model))
sink()

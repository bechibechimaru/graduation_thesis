warnings()
base_path <- "/Users/karubeshougo/Uni/seminar/graduation_thesis/graduation_thess"
survey_path <- "Statics/くらしと科学技術に関する意識調査/sources"
setwd(file.path(base_path, survey_path))

ra_data = read.csv("RawData_OpinionPoll_STIpolicy.csv", fileEncoding = "Shift_JIS")

# Q8_19で1または2を回答している人を抽出
positive_filtered_data <- ra_data[ra_data$Q8_19 %in% c(1, 2), ]
# Q8_19で4または5を回答している人を抽出
negative_filtered_data <- ra_data[ra_data$Q8_19 %in% c(4, 5), ]

# 年齢層ごとの集計
positive_age <- table(positive_filtered_data$F2)
negative_age <- table(negative_filtered_data$F2)

# データフレームの作成
age_data <- data.frame(
  Age = as.numeric(names(positive_age)),
  Positive = as.numeric(positive_age),
  Negative = as.numeric(negative_age)
)

# 相関分析
correlation <- cor.test(age_data$Positive, age_data$Negative)
print("相関分析の結果:")
print(correlation)

# 回帰分析
lm_model <- lm(Positive ~ Negative, data = age_data)
print("\n回帰分析の結果:")
print(summary(lm_model))

# 視覚化
library(ggplot2)

# 散布図の作成
p <- ggplot(age_data, aes(x = Negative, y = Positive)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "科学技術政策の重要度と年齢の相関",
       x = "重要政策ではないと答えた人数",
       y = "重要政策と答えた人数") +
  theme_minimal()

# グラフの保存
ggsave("Q8_19_age_correlation.png", p, width = 10, height = 6)

# 年齢層ごとの比率の計算
age_data$Total <- age_data$Positive + age_data$Negative
age_data$Positive_Ratio <- age_data$Positive / age_data$Total
age_data$Negative_Ratio <- age_data$Negative / age_data$Total

# 比率の視覚化
p2 <- ggplot(age_data, aes(x = Age)) +
  geom_line(aes(y = Positive_Ratio, color = "重要政策"), size = 1) +
  geom_line(aes(y = Negative_Ratio, color = "重要政策ではない"), size = 1) +
  labs(title = "年齢層ごとの回答比率",
       x = "年齢層",
       y = "比率",
       color = "回答") +
  theme_minimal() +
  scale_color_manual(values = c("重要政策" = "blue", "重要政策ではない" = "red"))

# グラフの保存
ggsave("Q8_19_age_ratio.png", p2, width = 10, height = 6)

# 結果の要約
print("\n年齢層ごとの回答比率:")
print(age_data[, c("Age", "Positive_Ratio", "Negative_Ratio")]) 
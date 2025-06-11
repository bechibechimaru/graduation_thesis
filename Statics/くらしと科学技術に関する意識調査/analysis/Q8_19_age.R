warnings()
setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis/graduation_thess/Statics/くらしと科学技術に関する意識調査/sources")

ra_data = read.csv("RawData_OpinionPoll_STIpolicy.csv", fileEncoding = "Shift_JIS")

# Q8_19で1または2を回答している人を抽出
true_filtered_data <- ra_data[ra_data$Q8_19 %in% c(1, 2), ]
# print("科学技術が重要政策だと答えた人のリストです")
# print(true_filtered_data)

false_filtered_data <- ra_data[ra_data$Q8_19 %in% c(3, 4), ]
# print("科学技術が重要政策ではないと答えた人のリストです")
# print(false_filtered_data)

# # 年齢層をまとめる
# information_communication_age <- table(filtered_data$F2)
# barplot(information_communication_age, main = "Age Distribution[Information and Communication Technology]", xlab = "Age", ylab = "Count")

# # 結果を表示
# print(information_communication_age)

# ロジスティック回帰分析用のデータ準備
# Q8_19を二値変数に変換（1,2を1、3,4を0に）
ra_data$policy_important <- ifelse(ra_data$Q8_19 %in% c(1, 2), 1, 0)

# ロジスティック回帰分析の実行
logit_model <- glm(policy_important ~ F2, 
                  data = ra_data, 
                  family = binomial(link = "logit"))

# 結果の表示
print("Results of Logistic Regression Analysis:")
print(summary(logit_model))

# オッズ比の計算と表示
odds_ratio <- exp(coef(logit_model))
print("\nOdds Ratio:")
print(odds_ratio)

# 予測確率の計算
ra_data$predicted_prob <- predict(logit_model, type = "response")

# 年齢層ごとの予測確率を計算
age_prob <- aggregate(predicted_prob ~ F2, data = ra_data, FUN = mean)
print("\nPredicted Probability by Age Group:")
print(age_prob)

# 視覚化
library(ggplot2)

# 年齢層ごとの予測確率をプロット
ggplot(age_prob, aes(x = F2, y = predicted_prob)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Probability of Considering Science and Technology Policy as Important by Age Group",
       x = "Age Group",
       y = "Predicted Probability") +
  theme_minimal()

# モデルの適合度を確認
print("\nModel Fit:")
print(anova(logit_model, test = "Chisq"))

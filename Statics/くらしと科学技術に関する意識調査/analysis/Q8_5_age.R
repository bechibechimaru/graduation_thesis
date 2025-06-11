warnings()
setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis/graduation_thess/Statics/くらしと科学技術に関する意識調査/sources")

ra_data = read.csv("RawData_OpinionPoll_STIpolicy.csv", fileEncoding = "Shift_JIS")


# ロジスティック回帰分析用のデータ準備
# Q8_5を二値変数に変換（1,2を1、3,4を0に）
ra_data$policy_important <- ifelse(ra_data$Q8_5 %in% c(1, 2), 1, 0)

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

# グラフの作成
library(ggplot2)

p <- ggplot(age_prob, aes(x = F2, y = predicted_prob)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Predicted Probability of Information and Communication Policy Importance by Age Group",
       x = "Age Group",
       y = "Predicted Probability") +
  theme_minimal()

ggsave("Q8_5_age_probability.png", p, path = "../sources", width = 10, height = 6)

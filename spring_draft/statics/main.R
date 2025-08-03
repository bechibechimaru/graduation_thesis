current_dir <- setwd("/Users/karubeshougo/Uni/seminar/graduation_thesis")
origin_data <- read.csv("Statics/くらしと科学技術に関する意識調査/sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", fileEncoding = "CP932", encoding = "UTF-8")

analysis_data <- data.frame(
# [従属変数]q8.5, q8.19 において、重要であると考えている
    is_important_q8_5 = ifelse(origin_data$Q8_5 == 1 | origin_data$Q8_5 == 2, 1, 0),
    is_important_q8_19 = ifelse(origin_data$Q8_19 == 1 | origin_data$Q8_19 == 2, 1, 0),
    
# [独立変数] 政府への信頼度
    trust_government = ifelse(origin_data$Q11_8 == 1 | origin_data$Q11_8 == 2, 1, 0)
)

# モデル1
# 政府への信頼が「情報通信政策」の重要度認識に与える影響
model_1 <- glm(is_important_q8_5 ~ trust_government, data = analysis_data, family = "binomial")
summary(model_1)

# モデル2
# 政府への信頼が「科学技術イノベーション政策」の重要度認識に与える影響
model_2 <- glm(is_important_q8_19 ~ trust_government, data = analysis_data, family = "binomial")
summary(model_2)

# 結果を図示する
library(ggplot2)
library(broom)

tidy_model_1 <- tidy(model_1, conf.int = TRUE)
tidy_model_2 <- tidy(model_2, conf.int = TRUE)

tidy_model_1$model <- "1:情報通信政策\n(Q8.5)"
tidy_model_2$model <- "2:科学技術イノベーション政策\n(Q8.19)"

combined_models_filtered <- rbind(tidy_model_1, tidy_model_2)

# 係数プロットを作成する
revised_plot <- ggplot(combined_models_filtered, aes(x = estimate, y = model, color = model)) +
    geom_point(size = 4) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
    title = "政府への信頼が\n各政策の重要度認識に与える影響",
    subtitle = "ロジスティック回帰分析の係数プロット（95%信頼区間）",
    x = "係数（Estimate）",
    y = "モデル"
    ) +
    theme_classic() +
    theme(legend.position = "none")

# 2. ggsave()で保存する際の横幅（width）を少し広げる
#    こうすることで、タイトル全体が切れることなく収まる
ggsave("my_coefficient_plot_revised.png",
    plot = revised_plot, 
    width = 10,  # 以前より大きくする
    height = 6,   # 以前より大きくする
    dpi = 300)

# ------------------------------------------------------------
# 交絡変数を定義する
age_labels <- c("16〜19歳", "20〜24歳", "25〜29歳", "30〜34歳", 
                "35〜39歳", "40〜44歳", "45〜49歳", "50〜54歳", 
                "55〜59歳", "60〜64歳", "65〜69歳", "70〜74歳", 
                "75〜79歳", "80歳以上")
analysis_data$gender <- factor(origin_data$F1, levels = 1:2, labels = c("男性", "女性"))
analysis_data$age <- factor(origin_data$F2, levels = 1:14, labels = age_labels)

# モデル1
# 政府への信頼が「情報通信政策」の重要度認識に与える影響
print("情報通信政策の係数プロット・交絡変数：性別")
model_1 <- glm(is_important_q8_5 ~ trust_government + gender - 1, data = analysis_data, family = "binomial")
summary(model_1)

# モデル2
# 政府への信頼が「科学技術イノベーション政策」の重要度認識に与える影響
print("科学技術イノベーション政策の係数プロット・交絡変数：性別")
model_2 <- glm(is_important_q8_19 ~ trust_government + gender - 1, data = analysis_data, family = "binomial")
summary(model_2)

# ------------------------------------------------------------

# モデル1
# 政府への信頼が「情報通信政策」の重要度認識に与える影響
print("情報通信政策の係数プロット・交絡変数：年齢")
model_1 <- glm(is_important_q8_5 ~ trust_government + age - 1, data = analysis_data, family = "binomial")
summary(model_1)

# モデル2
# 政府への信頼が「科学技術イノベーション政策」の重要度認識に与える影響
print("科学技術イノベーション政策の係数プロット・交絡変数：年齢を考慮した係数プロットを作成")
model_2 <- glm(is_important_q8_19 ~ trust_government + age - 1, data = analysis_data, family = "binomial")
summary(model_2)


# Q8_19を二値変数に変換（1,2を1、3,4を0に）
ra_data$policy_important <- ifelse(ra_data$Q8_19 %in% c(1, 2), 1, 0)
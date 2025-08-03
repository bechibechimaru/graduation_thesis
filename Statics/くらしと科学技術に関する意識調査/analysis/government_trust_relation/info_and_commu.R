warnings()
current_path <- "/Users/karubeshougo/Uni/seminar/graduation_thesis/graduation_thess/Statics/くらしと科学技術に関する意識調査/analysis/government_trust_relation"

setwd(current_path)

ra_data = read.csv("../../sources/origin_data/RawData_OpinionPoll_STIpolicy.csv", fileEncoding = "Shift_JIS")

print(ra_data["Q8_5"])

# 政府を信頼している人(Q11_8で1or2を回答している人)
trust_government_data <- ra_data[ra_data$Q11_8 %in% c(1, 2), ]
print("政府を信頼している人のデータ")
print(trust_government_data[Q8_5])



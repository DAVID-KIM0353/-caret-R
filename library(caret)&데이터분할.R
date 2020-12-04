library(caret) # 데이터 분할/전처리/튜닝/모델학습/평가 도와주는 라이브러리


# 데이터 분할하기
insurance <- read.csv('insurance.csv')
View(insurance) # 보험요금 : charges
train_index <- createDataPartition(y = insurance$charges, p = .6, list = FALSE)
train_data <- insurance[train_index, ]
test_data <- insurance[-train_index, ]


# 우리의 목표는 브랜드 로열티 사수!# 

customer <- read.csv('customer.csv')
purchase <- read.csv('purchase.csv')
items <- read.csv('items.csv')

library(dplyr)
library(ggplot2)
# df: 전체데이터, 데이터 합치기
df <- left_join(purchase, customer, by = 'cust_cd')
df <- left_join(df, items, by = 'goods_cd')
df1 <- na.omit(df)
head(df1)
sum(is.na(df1))
summary(df1$age)
#--------여기까지 1차 결측치 제거


# 나이대 함수
age <- function(x){
  return (floor(x/10) * 10)
}
age(34)
#########

df1$age[df1$age < 1] <- NA
sum(is.na(df1$age))
df3 <- sapply(df1[,10], function(x) floor(x/10) * 10)
df2 <- df1
df2$age <- df3
sum(is.na(df2$age)) # 238705 (1차 결측치 값)
#--------2차 결측치 제거
df2 <- na.omit(df2)
length(df2$age) # 238629 (2결측치 제거된 값)
head(df2)
summary(df2$age) # age 전처리 완료
View(df2) # <- age 전처리 완료된 data set

#----------EDA(데이터 탐색)
############
# grouping & visualizaion / 나이대별 구매금액 시각화
df2 %>% group_by(age) %>% summarise(total = sum(order_amt))
ggplot(data = df2, mapping = aes(x = age, y = order_amt))+
  geom_bar(stat = 'identity', mapping = aes(fill = age))
############
# 나이대별 사용한 유통채널 시각화
head(df2)
library(ggplot2)
ggplot(data = df2, mapping = aes(x = age))+
  geom_bar()+
  facet_wrap(~channel)
############
# 대분류별 중분류별 소분류별 확인하기
count(df2['goods_cd_L_nm'])
head(df2)
str(df2) # 데이터 type 확인

# 대, 중, 소 분류별 갯수 파악
class_all <- df2 %>% group_by(goods_cd_L_nm, goods_cd_M_nm, goods_cd_S_nm) %>% count()
View(class_all)

# 대분류별 갯수 파악
class_first <- df2 %>% group_by(goods_cd_L_nm) %>% count()
unique(class_first)
View(class_first)
sort_L <- unique(class_first)
sort_L <- sort_L[order(-sort_L$n),] # 대분류별 count 내림차순 정렬
sort_L # top5: 농산물, 축산물, 이미용품, 수산물, 건강기능식품

# 중분류별 갯수 파악
class_second <- df2 %>% group_by(goods_cd_M_nm) %>% count()
unique(class_second)
View(class_second)
sort_M <- unique(class_second)
sort_M <- sort_M[order(-sort_M$n),] # 중분류별 count 내림차순 정렬
sort_M # top5: 일반화장품, 국내산 미곡류, 위생용품, 국내산 돼지가공, 기타건강기능식품

# 소분류별 갯수 파악
class_third <- df2 %>% group_by(goods_cd_S_nm) %>% count()
unique(class_third)
View(class_third)
sort_S <- unique(class_third)
sort_S <- sort_S[order(-sort_S$n),] # 소분류별 count 내림차순 정렬
sort_S # top5: 기타, 쌀, 양념육, 청소용품, 기타추출가공

# 전체품목별 개수 
length(df2$goods_nm) # 품목 갯수: 238,629
class_name <- df2 %>% group_by(goods_nm) %>% count()
unique(class_name)
count(class_name) # unique한 품목 갯수: 23,410

# 성별 남자와 여자 매출추이
gender <- df2 %>% group_by(gender) %>% count()
gender
df3 <- filter(df2, gender %in% c('F', 'M')) # 'N'과, ' '은 제거하고 남은 행
df3 # 233449개
df3 %>% group_by(gender) %>% count()
df3 %>% group_by(gender) %>% summarise(total = sum(order_amt)) # 성별 별로 구매한 금액
ggplot(data = df3, mapping = aes(x = gender)) +
  geom_bar(mapping = aes(fill = gender)) # 본 데이터에 대한 성별 count
ggplot(data = df3, mapping = aes(x = gender, fill = ..prop..,y = ..prop.., group = 1)) +
  geom_bar() # 본 데이터에 대한 성별 비율, 여성: 80%, 남성: 20%로 여자가 압도적으로 많다.
# 따라서 우리는 age와 성별 데이터를 분석함으로 주부의 구매비율이 높다고 판단하였다.
# 주부를 대상으로 연관성분석 및 클러스터링을 통해 1+1 행사나 이벤트를 기획할 수 있는 분석을 제시한다.

# 지금부터 df3으로 분석(성별을 전처리 했기 때문)
# ---------여기까지 고객 segment

# 여성에 대한 분석
head(df3)
df4 <- filter(df2, gender %in% 'F')
head(df4) # 여성만 나오는 데이터
View(df4)

ggplot(data = df4, mapping = aes(x = reorder(age, order_amt,FUN=mean), y = order_amt)) +
  geom_boxplot(mapping = aes(color = age)) # 50대에서 특정 고객이 큰돈을 썻음

# 계절별로 할인행사를 진행한다 = 이에 구매금액이 크게 뛴다면 주부일 가능성 up!
# 그렇다면 우리는 이벤트를 어떻게 조정하면 좋을까?
# 계절별로 데이터 분석을 실시한다.

library(tidyverse)
library('lubridate')
df5 <- df4
df5$order_date <- ymd(df5$order_date) # 날짜 데이터 전처리
head(df5)
min(df5$order_date)
max(df5$order_date) # 2009-07-01 ~ 2011-06-30 까지의 데이터
# 각 년도마다 월별 매출 추이를 알아보겠다.

theme_set(theme_minimal()) #ggplot2 background annotation 최소화
ggplot(data = df5, aes(x = order_date, y = order_amt))+
  geom_line(color = "#00AFBB", size = 2)

#여러 개 시계열 그래프 그리기(psavert, uempmed)
ggplot(df5, aes(x = order_date, y = order_amt)) + 
  geom_line(color = 'green', size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+
  scale_x_date()


# 2009년 월별 매출
date_2009 <- subset(df5, order_date >= as.Date("2009-07-01") & order_date < as.Date("2010-01-01"))
p <- ggplot(date_2009, aes(x=order_date, y=order_amt)) + geom_line() 
p
datebreaks <- seq(as.Date("2009-07-01"), as.Date("2010-01-01"), by="2 month")
p + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))
g1 <- p + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))


# 2010년 월별 매출 
date_2010 <- subset(df5, order_date >= as.Date("2010-01-01") & order_date < as.Date("2011-01-01"))
p2 <- ggplot(date_2010, aes(x=order_date, y=order_amt)) + geom_line()
p2
datebreaks <- seq(as.Date("2010-01-01"), as.Date("2011-01-01"), by="2 month")
p2 + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))
g2 <- p2 + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))

# 2011년 월별 매출 
date_2011 <- subset(df5, order_date >= as.Date("2011-01-01") & order_date < as.Date("2011-06-30"))
p3 <- ggplot(date_2011, aes(x=order_date, y=order_amt)) + geom_line()
p3
datebreaks <- seq(as.Date("2011-01-01"), as.Date("2011-06-30"), by="2 month")
p3 + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))
g3 <- p3 + scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))
g3


# 인사이트: 나이대 => 40~50대, 남녀 => 여자:남자 = 8:2, 날짜데이터(여자만) => 설날 및 추석 매출이 높음
#           유통 => TV가 제일 많음 (남,여)

# 브랜드 로열티 : 40~50대의 여성 고객 잡기

head(df3) # 나이, 성별 전처리된 데이터
# 구매순서별 항목 찾기
df3 %>% group_by(order_seq) %>% count()
df_o <- filter(df3, order_seq %in% 1) # 물품순서로 첫번째로 쌀을 제일 많이 구매함
df_o %>% group_by(goods_cd_S_nm) %>% count() %>% arrange(desc(n)) 

df_o2 <- filter(df3, order_seq %in% 2)
df_o2 %>% group_by(goods_cd_S_nm) %>% count() %>% arrange(desc(n))

df_o3 <- filter(df3, order_seq %in% 3)
df_o3 %>% group_by(goods_cd_S_nm) %>% count() %>% arrange(desc(n))

head(df4)

length(df4$age)
unique(df4$age)
unique(df4$gender)


# 보험
i <- filter(df4, age %in% c(30, 40, 50))
i <- filter(i, gender %in% 'F')
df_i <- i
df_i <- filter(df_i, goods_cd_L_nm %in% '보험')
unique(df_i$goods_cd_M_nm) # 중분류로 선택

df2_f <- filter(df2, gender %in% 'F')
df2_f <- filter(df2_f, age>=30&age<=50)
df2_f <- filter(df2_f, goods_cd_L_nm == '보험')
df2_f


cart1 <- select(df5, c('order_date','channel','age','goods_cd_M_nm'))
cart1
head(cart1)
c <- rpart(goods_cd_M_nm~., data=cart1)
c

# 날짜 데이터 분기 나누기
length(df5$age)
head(cart1)
cart1$order_date[cart1$order_date]

head(cart1) 

q <- quarter(cart1$order_date)

cart1$order_date <- q
head(cart1) # 의사결정나무 데이터

cart1 <- cart1[!(cart1$goods_cd_M_nm == "현대해상" ), ] # 현대해상 제거

# 보험 의사결정나무*******
inTrain <- createDataPartition(y = cart1$goods_cd_M_nm, p = 0.6, list = FALSE)
train <- cart1[inTrain, ]
test <- cart1[-inTrain, ]

fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10)

train

tree.model <- train(goods_cd_M_nm ~ ., data = train, method = "rpart",trControl = fitControl)
tree.model

plot(tree.model)
predictions <- predict(tree.model, test)
predictions_f <- as.factor(predictions)
test_f <- as.factor(test$goods_cd_M_nm)
confusionMatrix(predictions_f, test_f)

library(rattle)
fancyRpartPlot(tree.model$finalModel)

plot(tree.model$finalModel)
text(tree.model $finalModel)

library(Metrics)
predictions <- predict(tree.model, test)
accuracy(test$goods_cd_M_nm, predictions)
#############################################
# 랜덤포레스트
customGrid <- expand.grid(mtry = 1:10)
train
rf.model <- train(goods_cd_M_nm ~ ., data = train, method = "rf",trControl = fitControl, tuneGrid = customGrid)
rf.model

predictions <- predict(rf.model, test)




############################
library(tidyverse)
library('lubridate')
df5 <- df2_f
df5$order_date <- ymd(df5$order_date) # 날짜 데이터 전처리
head(df5)

############################

i <- filter(df2, age %in% c(30, 40, 50))
i <- filter(i, gender %in% 'F')
df_i <- i
df_i <- filter(df_i, goods_cd_L_nm %in% '보험')
unique(df_i$goods_cd_M_nm)


# 중분류 보험별 시각화
# df_i %>% group_by(goods_cd_M_nm) %>% count() %>% arrange(desc(n)) %>% sort()
ggplot(data = df_i, mapping = aes(x = goods_cd_M_nm, fill = goods_cd_M_nm))+
  geom_bar()+
  coord_flip()

df_i %>% group_by(goods_cd_M_nm) %>% count()
df_i_1 <- df_i[!(df_i$goods_cd_M_nm == "현대해상" ), ]


# 의사결정 나무(알고리즘 분석)
cart1 <- select(df_i_1, c('channel','age','order_seq','goods_cd_M_nm'))
head(cart1)
library(caret)
library(rpart)


# k-means
k <- cart1
k$channel[k$channel== 'TV'] <- 1
k$channel[k$channel== 'WEB'] <- 2
k$channel[k$channel== 'BOOK'] <- 3


k$goods_cd_M_nm[k$goods_cd_M_nm == '차티스 손해보험'] <- 'A'
k$goods_cd_M_nm[k$goods_cd_M_nm == '에이스화재해상'] <- 'B'
k$goods_cd_M_nm[k$goods_cd_M_nm == '삼성화재'] <- 'C'
k$goods_cd_M_nm[k$goods_cd_M_nm == '삼성생명'] <- 'D'
k$goods_cd_M_nm[k$goods_cd_M_nm == '롯데손해보험'] <- 'E'
k$goods_cd_M_nm[k$goods_cd_M_nm == '라이나 생명'] <- 'F'
k$goods_cd_M_nm[k$goods_cd_M_nm == '동부화재'] <- 'G'
k$goods_cd_M_nm[k$goods_cd_M_nm == '대한생명'] <- 'H'
k$goods_cd_M_nm[k$goods_cd_M_nm== '금호생명'] <- 'I'
k$goods_cd_M_nm[k$goods_cd_M_nm== 'LIG손해보험'] <- 'J'
k$goods_cd_M_nm[k$goods_cd_M_nm== 'ING생명'] <- 'K'
k$goods_cd_M_nm[k$goods_cd_M_nm== 'GA파트너'] <- 'L'
k$goods_cd_M_nm[k$goods_cd_M_nm== 'AIA생명'] <- 'O'


# --------------------------------------------------

head(df2)
length(df5$age)
s <- subset(df5, order_date >= as.Date("2010-01-10") & order_date < as.Date("2010-01-25"))
m1 <- s %>% group_by(goods_cd_S_nm) %>% count()
View(m1)

s <- subset(df5, order_date >= as.Date("2010-01-25") & order_date < as.Date("2010-02-10"))
m2 <- s %>% group_by(goods_cd_S_nm) %>% count()
View(m2)

s <- subset(df5, order_date >= as.Date("2010-02-10") & order_date < as.Date("2010-02-25"))
m3 <- s %>% group_by(goods_cd_S_nm) %>% summarise(total = sum(order_amt))
View(m3)
# ---------------------------------------------------
###########################
# 의사결정나무(Channel)
library(caret)
library(rpart)
head(df5) # 여성만 추출한 데이터
View(df5)
aa <- df5[df5$age >= 30 & df5$age <= 50, ]
head(aa) # 여성과 30~50대로 추출된 데이터
q <- quarter(aa$order_date) # 연/월/일 으로 이루어진 데이터를 분기(type: int)로 변환
aa$order_date <- q
aaa <- aa


aaa$goods_cd_L_nm <- as.factor(aaa$goods_cd_L_nm)
# 변수선택
aa1 <- select(aaa, c('order_date', 'channel', 'order_amt', 'age', 'goods_cd_L_nm'))
View(aa1)
inTrain <- createDataPartition(y = aa1$channel, p = 0.6, list = FALSE)
train <- aa1[inTrain, ]
test <- aa1[-inTrain, ]

fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10)

tree.model <- train(channel ~ ., data = train, method = "rpart",trControl = fitControl)
tree.model
predictions <- predict(tree.model, test)

library(rattle)
fancyRpartPlot(tree.model$finalModel)

library(Metrics)
accuracy(test$channel, predictions)
# ---------------------------------------------
aa1 <- select(aaa, c('order_date', 'channel', 'order_amt', 'age'))
View(aa1)

inTrain <- createDataPartition(y = aa1$channel, p = 0.6, list = FALSE)
train <- aa1[inTrain, ]
test <- aa1[-inTrain, ]

fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10)

train

tree.model <- train(channel ~ ., data = train, method = "rpart",trControl = fitControl)
tree.model

predictions <- predict(tree.model, test)
predictions_f <- as.factor(predictions)
test_f <- as.factor(test$goods_cd_M_nm)
confusionMatrix(predictions_f, test_f)

library(rattle)
fancyRpartPlot(tree.model$finalModel)




#####################################

plot(tree.model)
predictions <- predict(tree.model, test)
predictions_f <- as.factor(predictions)
test_f <- as.factor(test$goods_cd_M_nm)
confusionMatrix(predictions_f, test_f)

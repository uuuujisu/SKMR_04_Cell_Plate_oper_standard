rm(list=ls())

# 패키지  -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(stats)) install.packages('stats'); require(stats)
if(!require(caret)) install.packages('caret'); require(caret)
if(!require(rpart)) install.packages('rpart'); require(rpart)
if(!require(rpart.plot)) install.packages('rpart.plot'); require(rpart.plot)
if(!require(ROSE)) install.packages('ROSE'); require(ROSE)


#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Rdata load
load(file= "../0.Data/NF3_N3_GuideLine/df.Rdata")

# 1h/2h/3h/...
t_cut <-c()
for (i in 1:48){
  t_cut[i] <- 2881 - 60*i
}

# df_3h -------------------------------------------------------------------


df_3h <- df %>%
  filter(t >= t_cut[3]) %>%
  group_by(File_num,Item_No) %>%
  mutate(전류load_3h = my.mode(전류load)) %>% 
  mutate(평균IR_시계열SD_3h = sd(IR_Plates_mean,na.rm=TRUE),최대IR_시계열SD_3h = sd(IR_Plates_max,na.rm=TRUE),
         IR플레이트_군집SD_평균_3h = mean(IR_Plates_sd,na.rm=TRUE), IR플레이트_군집SD_최대_3h = my.max(IR_Plates_sd), 
         평균IR_시계열RANGE_3h = my.range(IR_Plates_mean),최대IR_시계열RANGE_3h = my.range(IR_Plates_max),
         IR플레이트_군집RANGE_평균_3h = mean(IR_Plates_range,na.rm=TRUE), IR플레이트_군집RANGE_최대_3h = my.max(IR_Plates_range), 
         IR_평균_3h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(평균PI_시계열SD_3h = sd(PI_Plates_mean,na.rm=TRUE),최대PI_시계열SD_3h = sd(PI_Plates_max,na.rm=TRUE),
         PI플레이트_군집SD_평균_3h = mean(PI_Plates_sd,na.rm=TRUE), PI플레이트_군집SD_최대_3h = my.max(PI_Plates_sd), 
         평균PI_시계열RANGE_3h = my.range(PI_Plates_mean),최대PI_시계열RANGE_3h = my.range(PI_Plates_max),
         PI플레이트_군집RANGE_평균_3h = mean(PI_Plates_range,na.rm=TRUE), PI플레이트_군집RANGE_최대_3h = my.max(PI_Plates_range), 
         PI_평균_3h = mean(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(Cell액높이_차압식_평균_3h = mean(Cell액높이_차압식,na.rm=TRUE), Cell액높이_차압식_SD_3h = sd(Cell액높이_차압식,na.rm=TRUE), 
         Cell액높이_차압식_RANGE_3h = my.range(Cell액높이_차압식)) %>%
  mutate(압력차이_평균_3h = mean(압력차이,na.rm=TRUE), 압력차이_SD_3h = sd(압력차이,na.rm=TRUE),
         압력차이_RANGE_3h = my.range(압력차이)) %>% 
  mutate(음극_압력_평균_3h = mean(음극_압력,na.rm=TRUE), 음극_압력_SD_3h = sd(음극_압력,na.rm=TRUE),
         음극_압력_RANGE_3h = my.range(음극_압력)) %>%
  mutate(양극_압력_평균_3h = mean(양극_압력,na.rm=TRUE), 양극_압력_SD_3h = sd(양극_압력,na.rm=TRUE),
         양극_압력_RANGE_3h = my.range(양극_압력)) %>%
  mutate(CW_CV_Opening_평균_3h = mean(CW_CV_Opening,na.rm=TRUE), CW_CV_Opening_SD_3h = sd(CW_CV_Opening,na.rm=TRUE),
         CW_CV_Opening_RANGE_3h = my.range(CW_CV_Opening)) %>%
  mutate(STM_CV_Opening_평균_3h = mean(STM_CV_Opening,na.rm=TRUE), STM_CV_Opening_SD_3h = sd(STM_CV_Opening,na.rm=TRUE),
         STM_CV_Opening_RANGE_3h = my.range(STM_CV_Opening)) %>%
  mutate(복도측액_온도_평균_3h = mean(복도측액_온도,na.rm=TRUE), 복도측액_온도_SD_3h = sd(복도측액_온도,na.rm=TRUE),
         복도측액_온도_RANGE_3h = my.range(복도측액_온도)) %>%
  mutate(유틸리티측액_온도_평균_3h = mean(유틸리티측액_온도,na.rm=TRUE), 유틸리티측액_온도_SD_3h = sd(유틸리티측액_온도,na.rm=TRUE),
         유틸리티측액_온도_RANGE_3h = my.range(유틸리티측액_온도)) %>%  
  mutate(양극집합관_온도_평균_3h = mean(양극집합관_온도,na.rm=TRUE), 양극집합관_온도_SD_3h = sd(양극집합관_온도,na.rm=TRUE),
         양극집합관_온도_RANGE_3h = my.range(양극집합관_온도)) %>%
  mutate(음극집합관_온도_평균_3h = mean(음극집합관_온도,na.rm=TRUE), 음극집합관_온도_SD_3h = sd(음극집합관_온도,na.rm=TRUE),
         음극집합관_온도_RANGE_3h = my.range(음극집합관_온도)) %>%
  mutate(전압_평균_3h = mean(전압,na.rm=TRUE), 전압_SD_3h = sd(전압,na.rm=TRUE), 전압_RANGE_3h = my.range(전압)) %>%
  ungroup
df_3h <- df_3h[,c(1:4,32:83)] %>% arrange(File_num,Item_No) %>% distinct()

# n=2,292
df_3h[sapply(df_3h,is.infinite)] <- NA
df_3h[sapply(df_3h,is.nan)] <- NA
NArow <- which(is.na(df_3h),arr.ind=TRUE)
df_3h <- df_3h[-NArow[,1],]

df_3h$y <- as.factor(df_3h$y)

# 모집단 class  -------------------------------------------------------------

# 5500-7500 -> LOW
# 7500-9500 -> Middle
# 9500 이상 -> High
hist(df_3h$전류load_3h,breaks=26)

df_3h$전류load그룹_3h[df_3h$전류load_3h >=5500 & df_3h$전류load_3h < 7500 ] = "LOW"
df_3h$전류load그룹_3h[df_3h$전류load_3h >=7500 & df_3h$전류load_3h < 9500 ] = "MID"
df_3h$전류load그룹_3h[df_3h$전류load_3h >=9500 ] = "HIGH"

load_low <- df_3h %>% filter(전류load그룹_3h =="LOW")
load_middle <- df_3h %>% filter(전류load그룹_3h =="MID")
load_high <- df_3h %>% filter(전류load그룹_3h =="HIGH")


# Oversampling --------------------------------------------------------------

set.seed(222)
# low
table(load_low$y)
load_low_over <- ovun.sample(y~., data = load_low, method = "over", N=1000)$data
table(load_low_over$y)
# middle
table(load_middle$y)
load_middle_over <- ovun.sample(y~., data = load_middle, method = "over", N=800)$data
table(load_middle_over$y)
#high
table(load_high$y)
load_high_over <- ovun.sample(y~., data = load_high, method = "over", N=1000)$data
table(load_high_over$y)

#save(load_low_over, load_middle_over, load_high_over, file= "../0.Data/NF3_N3_GuideLine/oversampling.Rdata")


# CW tree -----------------------------------------------------------------


#low
tmp <- load_low_over %>% select(c(y,CW_CV_Opening_SD_3h))

my.control<-rpart.control(xval=10, minsplit=160)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)
load_low_over$pred.y <- pred

#mid
tmp <- load_middle_over %>% select(c(y,CW_CV_Opening_SD_3h))

my.control<-rpart.control(xval=10,minsplit=310)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)
load_middle_over$pred.y <- pred


#high
tmp <- load_high_over %>% select(c(y,CW_CV_Opening_SD_3h))

my.control<-rpart.control(xval=10,minsplit=400)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)


# CW autocorrelation ------------------------------------------------------

CW_lag_3h <- df %>% filter(t >= t_cut[3]) %>%
  select(File_num,Item_No,y_date,y,Time,t,CW_CV_Opening)
NArow <- which(is.na(CW_lag_3h),arr.ind=TRUE)
CW_lag_3h <- CW_lag_3h[-NArow[,1],]

#5분 주기 데이터
lag5 <-c()
for (i in 1:36){
  lag5[i] <- 2881- 5*(i-1)
}
lag5  

#94,201
CW_lag5_3h <- CW_lag_3h %>%
  filter(t %in% lag5) %>%
  group_by(File_num,Item_No) %>%
  mutate(SD=sd(CW_CV_Opening)) %>%
  ungroup

#3시간 내내 개도율 일정하면 삭제
#65,287
CW_lag5_3h <- CW_lag5_3h %>%
  filter(SD!=0)

acfdf <- CW_lag5_3h %>% select(File_num,Item_No) %>% distinct %>%
  mutate(group=row_number()) 

CW_lag5_3h <- CW_lag5_3h %>%
  left_join(acfdf,by=c("File_num","Item_No"))

#확인
# aa<- CW_lag5_3h %>% filter(File_num==10,Item_No=="G-E")
# z <- acf(aa$CW_CV_Opening, type=c("correlation"), plot=TRUE)
# cycle <- (which(z$acf==min(z$acf))-1)*5
# cycle

acfdf$cycle <-NA

i<-1
for(i in 1:nrow(acfdf)){
  tmp <- CW_lag5_3h[CW_lag5_3h$group==i,]
  z <- acf(tmp$CW_CV_Opening, type=c("correlation"))
  cycle <- (which(z$acf==min(z$acf))-1)*5
  acfdf$cycle[i] <-cycle
}

df_3h <- df_3h %>% left_join(acfdf %>% select(-group), by=c("File_num","Item_No"))



# CW cycle ----------------------------------------------------------------

#load(file= "../0.Data/NF3_N3_GuideLine/oversampling.Rdata")

# CW tree

tmp <- load_low_over %>%  left_join(acfdf %>% select(-group), by=c("File_num","Item_No")) %>%
  select(c(y,cycle))
#tmp$cycle[is.na(tmp$cycle)] <- 0

my.control<-rpart.control(xval=10,minsplit=0)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)
load_low_over$pred.y <- pred

#mid
tmp <- load_middle_over  %>%  left_join(acfdf %>% select(-group), by=c("File_num","Item_No")) %>%
  select(c(y,cycle))
#tmp$cycle[is.na(tmp$cycle)] <- 0

my.control<-rpart.control(xval=10,minsplit=10)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)
load_middle_over$pred.y <- pred


#high
tmp <- load_high_over  %>%  left_join(acfdf %>% select(-group), by=c("File_num","Item_No")) %>%
  select(c(y,cycle))
#tmp$cycle[is.na(tmp$cycle)] <- 0

my.control<-rpart.control(xval=10,minsplit=400)
tree <- rpart(y ~ ., data=tmp,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=tmp, type = "class")
confusionMatrix(pred, tmp$y)


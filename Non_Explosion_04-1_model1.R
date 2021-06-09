rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(glmnet)) install.packages('glmnet'); require(glmnet)
if(!require(caret)) install.packages('caret'); require(caret)


#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Rdata load
load(file="../0.Data/N3_RData/prep02_data.Rdata")

colnames(prep02_df)
key <- colnames(prep02_df)[1:4]
IRs <- colnames(prep02_df)[8:32]
PIs <- colnames(prep02_df)[35:59]
Plates <- colnames(prep02_df)[72:96]

load(file="../0.Data/N3_RData/summary_data.Rdata")

# 01. dataset1 ------------------------------------------------------------

tmp_1h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_1h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_1h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_1h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_1h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_1h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_1h','IR_VE_1h','PI_EV_1h','PI_VE_1h','IR_mean_1h')) %>%
  distinct()%>%
  ungroup

# 30*89 - (전처리 오류 삭제됨) -> n = 2634

tmp_3h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="3h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_3h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_3h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_3h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_3h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_3h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_3h','IR_VE_3h','PI_EV_3h','PI_VE_3h','IR_mean_3h')) %>%
  distinct()%>%
  ungroup

tmp_6h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="6h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_6h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_6h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_6h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_6h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_6h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_6h','IR_VE_6h','PI_EV_6h','PI_VE_6h','IR_mean_6h')) %>%
  distinct()%>%
  ungroup

tmp_12h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="12h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_12h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_12h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_12h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_12h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_12h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_12h','IR_VE_12h','PI_EV_12h','PI_VE_12h','IR_mean_12h')) %>%
  distinct()%>%
  ungroup

tmp_24h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="24h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_24h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_24h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_24h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_24h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_24h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_24h','IR_VE_24h','PI_EV_24h','PI_VE_24h','IR_mean_24h')) %>%
  distinct()%>%
  ungroup

tmp_48h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_sd','PI_Plates_mean','PI_Plates_sd')) %>%
  filter(before_h=="48h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_48h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_VE_48h = var(IR_Plates_mean,na.rm=TRUE)) %>%
  mutate(PI_EV_48h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_VE_48h = var(PI_Plates_mean,na.rm=TRUE)) %>%
  mutate(IR_mean_48h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
  select(c(key,'IR_EV_48h','IR_VE_48h','PI_EV_48h','PI_VE_48h','IR_mean_48h')) %>%
  distinct()%>%
  ungroup

summary_2day <- tmp_1h %>% 
  full_join(tmp_3h, by = key) %>% 
  full_join(tmp_6h, by = key) %>% 
  full_join(tmp_12h, by = key) %>%
  full_join(tmp_24h, by = key) %>%
  full_join(tmp_48h, by = key) %>% 
  arrange(File_num,Item_No) 
#aa <- colnames(summary_1day)[5:29]

summary_2day$y <- as.factor(summary_2day$y)

# 결측 제거 2,634 -> 2,629
NArow <- which(is.na(summary_2day),arr.ind=TRUE)
summary_2day <- summary_2day[-NArow[,1],]


save(summary_2day, file="../0.Data/N3_RData/summary_2day_data.Rdata")
#load("../0.Data/N3_RData/summary_2day_data.Rdata")

# 02. modeling ------------------------------------------------------------

# split train & test set to 8:2
set.seed(210607)

samples <- sample(1:nrow(summary_2day),size=0.8*nrow(summary_2day),replace=F)
train <- summary_2day[samples,]
test <- summary_2day[-samples,]

# model.A : 1day info
# model.A <- glm(y ~ IR_EV_1h +	IR_VE_1h  +	PI_EV_1h   +	PI_VE_1h    +	IR_mean_1h   
#               +	IR_EV_3h  +	IR_VE_3h    +	PI_EV_3h  +	PI_VE_3h  +	IR_mean_3h  
#               +	IR_EV_6h  +	IR_VE_6h  +	PI_EV_6h  +	PI_VE_6h  +	IR_mean_6h 
#               +	IR_EV_12h +	IR_VE_12h  +	PI_EV_12h  +	PI_VE_12h  +	IR_mean_12h   
#               +	IR_EV_24h  +	IR_VE_24h +	PI_EV_24h +	PI_VE_24h  +	IR_mean_24h 
#               ,data = train, family=binomial(link=logit) )
# summary(model.A)
# 
# test$pred.A <- predict(model.A,newdata = test,type="response")
# summary(test$pred.A)
# test$pred.A.y <- ifelse(test$pred.A >=0.1,1,0)
# table(test$pred.A.y)
# table(test$y,test$pred.A.y)

# model.B : 2day info

model.B <- glm(y ~ IR_EV_1h +	IR_VE_1h  +	PI_EV_1h   +	PI_VE_1h    +	IR_mean_1h   
              +	IR_EV_3h  +	IR_VE_3h    +	PI_EV_3h  +	PI_VE_3h  +	IR_mean_3h  
              +	IR_EV_6h  +	IR_VE_6h  +	PI_EV_6h  +	PI_VE_6h  +	IR_mean_6h 
              +	IR_EV_12h +	IR_VE_12h  +	PI_EV_12h  +	PI_VE_12h  +	IR_mean_12h   
              +	IR_EV_24h  +	IR_VE_24h +	PI_EV_24h +	PI_VE_24h  +	IR_mean_24h
              +	IR_EV_48h  +	IR_VE_48h +	PI_EV_48h +	PI_VE_48h  +	IR_mean_48h
              ,data = train, family=binomial(link=logit) )
summary(model.B)

test$pred.B <- predict(model.B,newdata = test,type="response")
summary(test$pred.B)
test$pred.B.y <- ifelse(test$pred.B >=0.1,1,0)
table(test$pred.B.y)
table(test$y,test$pred.B.y)
 
# train$aa <- predict(model.B, newdata=train,type = "response")
# train$b <- ifelse(train$aa >=0.1,1,0)
# table(train$y,train$b)

# model.C : var
# 
# model.C <- glm(y ~ 	PI_EV_1h   +	PI_VE_1h    +	IR_mean_1h   
#                +		PI_EV_3h  +	PI_VE_3h  +	IR_mean_3h  
#                +		PI_EV_6h  +	PI_VE_6h  +	IR_mean_6h 
#                +		PI_EV_12h  +	PI_VE_12h  +	IR_mean_12h   
#                +		PI_EV_24h +	PI_VE_24h  +	IR_mean_24h
#                +		PI_EV_48h +	PI_VE_48h  +	IR_mean_48h
#                ,data = train, family=binomial(link=logit) )
# summary(model.C)
# 
# test$pred.C <- predict(model.C,newdata = test,type="response")
# summary(test$pred.C)
# #test$pred.C.y <- ifelse(test$pred.C >=0.5,1,0)
# test$pred.C.y <- ifelse(test$pred.C >=0.1,1,0)
# table(test$pred.C.y)
# table(test$y,test$pred.C.y)




# 03. variable selection --lasso ------------------------------------------


# split train & test set to 8:2
# set.seed(210608)
# set.seed(2021)
# 
# samples <- sample(1:nrow(summary_2day),size=0.8*nrow(summary_2day),replace=F)
# train <- summary_2day[samples,]
# test <- summary_2day[-samples,]

# LASSO - L1 norm

x <- train %>% select(-key) %>% data.matrix()
y <- train$y
x.test<- test %>% select(-c(key,'pred.B','pred.B.y')) %>% data.matrix()


# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)

# model
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = 0.006)
summary(model)
coef(model)

# model predict

test$pred.lasso <- predict(model ,newx=x.test, type="response")
summary(test$pred.lasso)
test$pred.lasso.y <- ifelse(test$pred.lasso >=0.05,1,0)
table(test$pred.lasso.y)
table(test$y,test$pred.lasso.y)

# 
# model.lasso <- glm(y ~ IR_EV_1h + IR_VE_1h + PI_VE_1h + IR_mean_1h 
#                    + PI_VE_6h + PI_EV_12h + PI_VE_24h + IR_VE_48h
#                    , data = train, family = binomial)
# summary(model.lasso)
# 
# test$pred.lasso <- predict(model.lasso, newdata = test,type="response")
# summary(test$pred.lasso)
# test$pred.lasso.y <- ifelse(test$pred.lasso >=0.1,1,0)
# table(test$pred.lasso.y)
# table(test$y,test$pred.lasso.y)


save(train,test, file="../0.Data/N3_RData/model1_data.Rdata")

# 04. xgboost -------------------------------------------------------------








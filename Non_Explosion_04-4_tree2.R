rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(glmnet)) install.packages('glmnet'); require(glmnet)

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


my.min <- function(x) ifelse( all(is.na(x)), NA, min(x, na.rm=T))
my.max <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T))
my.med <- function(x) ifelse( all(is.na(x)), NA, median(x, na.rm=T))

smrs <- c('IR_mean_1h', 'IR_sd_1h' ,'IR_med_1h', 'IR_min_1h', 'IR_max_1h', 'IR_range_1h',
          'PI_mean_1h', 'PI_sd_1h',  'PI_med_1h', 'PI_min_1h',  'PI_max_1h', 'PI_range_1h')
smrs3 <- c('IR_mean_3h', 'IR_sd_3h' ,'IR_med_3h', 'IR_min_3h', 'IR_max_3h', 'IR_range_3h',
          'PI_mean_3h', 'PI_sd_3h',  'PI_med_3h', 'PI_min_3h',  'PI_max_3h', 'PI_range_3h')
smrs6 <- c('IR_mean_6h', 'IR_sd_6h' ,'IR_med_6h', 'IR_min_6h', 'IR_max_6h', 'IR_range_6h',
          'PI_mean_6h', 'PI_sd_6h',  'PI_med_6h', 'PI_min_6h',  'PI_max_6h', 'PI_range_6h')
smrs12 <- c('IR_mean_12h', 'IR_sd_12h' ,'IR_med_12h', 'IR_min_12h', 'IR_max_12h', 'IR_range_12h',
          'PI_mean_12h', 'PI_sd_12h',  'PI_med_12h', 'PI_min_12h',  'PI_max_12h', 'PI_range_12h')
smrs24 <- c('IR_mean_24h', 'IR_sd_24h' ,'IR_med_24h', 'IR_min_24h', 'IR_max_24h', 'IR_range_24h',
          'PI_mean_24h', 'PI_sd_24h',  'PI_med_24h', 'PI_min_24h',  'PI_max_24h', 'PI_range_24h')
smrs48 <- c('IR_mean_48h', 'IR_sd_48h' ,'IR_med_48h', 'IR_min_48h', 'IR_max_48h', 'IR_range_48h',
          'PI_mean_48h', 'PI_sd_48h',  'PI_med_48h', 'PI_min_48h',  'PI_max_48h', 'PI_range_48h')




# 01. dataset2 ------------------------------------------------------------

tmp <- tmp_hour %>%
  left_join(prep02_df %>% select(key,'Time','t',IRs, PIs),by=c(key,'Time','t'))


#1h
tmp_1h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="1h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_1h = mean(IR,na.rm=T),
           IR_sd_1h = sd(IR,na.rm=T),
           IR_med_1h = my.med(IR),
           IR_min_1h = my.min(IR),
           IR_max_1h = my.max(IR),
           IR_range_1h = my.max(IR)-my.min(IR),
           PI_mean_1h = mean(PI,na.rm=T),
           PI_sd_1h = sd(PI,na.rm=T),
           PI_med_1h = my.med(PI),
           PI_min_1h = my.min(PI),
           PI_max_1h = my.max(PI),
           PI_range_1h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs) %>%
    distinct() 
  tmp_1h <- rbind(tmp_1h,aa)
}

tmp_1h <- tmp_1h %>%
  arrange(File_num, Item_No, Plate)

#2634 * 25

  
#3h
tmp_3h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="3h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_3h = mean(IR,na.rm=T),
           IR_sd_3h = sd(IR,na.rm=T),
           IR_med_3h = my.med(IR),
           IR_min_3h = my.min(IR),
           IR_max_3h = my.max(IR),
           IR_range_3h = my.max(IR)-my.min(IR),
           PI_mean_3h = mean(PI,na.rm=T),
           PI_sd_3h = sd(PI,na.rm=T),
           PI_med_3h = my.med(PI),
           PI_min_3h = my.min(PI),
           PI_max_3h = my.max(PI),
           PI_range_3h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs3) %>%
    distinct() 
  tmp_3h <- rbind(tmp_3h,aa)
}

tmp_3h <- tmp_3h %>%
  arrange(File_num, Item_No, Plate)

#6h
tmp_6h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="6h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_6h = mean(IR,na.rm=T),
           IR_sd_6h = sd(IR,na.rm=T),
           IR_med_6h = my.med(IR),
           IR_min_6h = my.min(IR),
           IR_max_6h = my.max(IR),
           IR_range_6h = my.max(IR)-my.min(IR),
           PI_mean_6h = mean(PI,na.rm=T),
           PI_sd_6h = sd(PI,na.rm=T),
           PI_med_6h = my.med(PI),
           PI_min_6h = my.min(PI),
           PI_max_6h = my.max(PI),
           PI_range_6h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs6) %>%
    distinct() 
  tmp_6h <- rbind(tmp_6h,aa)
}

tmp_6h <- tmp_6h %>%
  arrange(File_num, Item_No, Plate)

#12h
tmp_12h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="12h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_12h = mean(IR,na.rm=T),
           IR_sd_12h = sd(IR,na.rm=T),
           IR_med_12h = my.med(IR),
           IR_min_12h = my.min(IR),
           IR_max_12h = my.max(IR),
           IR_range_12h = my.max(IR)-my.min(IR),
           PI_mean_12h = mean(PI,na.rm=T),
           PI_sd_12h = sd(PI,na.rm=T),
           PI_med_12h = my.med(PI),
           PI_min_12h = my.min(PI),
           PI_max_12h = my.max(PI),
           PI_range_12h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs12) %>%
    distinct() 
  tmp_12h <- rbind(tmp_12h,aa)
}

tmp_12h <- tmp_12h %>%
  arrange(File_num, Item_No, Plate)



#24h
tmp_24h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="24h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_24h = mean(IR,na.rm=T),
           IR_sd_24h = sd(IR,na.rm=T),
           IR_med_24h = my.med(IR),
           IR_min_24h = my.min(IR),
           IR_max_24h = my.max(IR),
           IR_range_24h = my.max(IR)-my.min(IR),
           PI_mean_24h = mean(PI,na.rm=T),
           PI_sd_24h = sd(PI,na.rm=T),
           PI_med_24h = my.med(PI),
           PI_min_24h = my.min(PI),
           PI_max_24h = my.max(PI),
           PI_range_24h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs24) %>%
    distinct() 
  tmp_24h <- rbind(tmp_24h,aa)
}

tmp_24h <- tmp_24h %>%
  arrange(File_num, Item_No, Plate)



#48h
tmp_48h <- NULL
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="48h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_48h = mean(IR,na.rm=T),
           IR_sd_48h = sd(IR,na.rm=T),
           IR_med_48h = my.med(IR),
           IR_min_48h = my.min(IR),
           IR_max_48h = my.max(IR),
           IR_range_48h = my.max(IR)-my.min(IR),
           PI_mean_48h = mean(PI,na.rm=T),
           PI_sd_48h = sd(PI,na.rm=T),
           PI_med_48h = my.med(PI),
           PI_min_48h = my.min(PI),
           PI_max_48h = my.max(PI),
           PI_range_48h = my.max(PI)-my.min(PI)) %>%
    ungroup %>%
    select(key,'Plate', smrs48) %>%
    distinct() 
  tmp_48h <- rbind(tmp_48h,aa)
}

tmp_48h <- tmp_48h %>%
  arrange(File_num, Item_No, Plate)


#Join

summary_plates <- tmp_1h %>% 
  full_join(tmp_3h, by = c(key,'Plate')) %>% 
  full_join(tmp_6h, by = c(key,'Plate')) %>% 
  full_join(tmp_12h, by = c(key,'Plate')) %>%
  full_join(tmp_24h, by = c(key,'Plate')) %>%
  full_join(tmp_48h, by = c(key,'Plate')) %>% 
  arrange(File_num,Item_No) 


summary_plates$y <- as.factor(summary_plates$y)


# 결측 제거 65,850 -> 65,548
NArow <- which(is.na(summary_plates),arr.ind=TRUE)
summary_plates <- summary_plates[-NArow[,1],]


save(summary_plates, file="../0.Data/N3_RData/summary_plates_data.Rdata")
load("../0.Data/N3_RData/summary_plates_data.Rdata")

# 02. modeling ------------------------------------------------------------
key <- colnames(summary_plates)[1:4]
ranges <- grep("range",colnames(summary_plates))

# split train & test set to 8:2
set.seed(210607)

samples <- sample(1:nrow(summary_plates),size=0.8*nrow(summary_plates),replace=F)
train <- summary_plates[samples,]
test <- summary_plates[-samples,]
x.test<- test %>% select(-c(key,'Plate',ranges)) %>% data.matrix()
                         

# # model.A : mean sd med range
# model.A <- glm(y ~ IR_mean_1h + IR_sd_1h + IR_med_1h  + IR_range_1h 
#                + PI_mean_1h + PI_sd_1h + PI_med_1h +  PI_range_1h 
#                + IR_mean_3h + IR_sd_3h + IR_med_3h +  IR_range_3h 
#                + PI_mean_3h + PI_sd_3h + PI_med_3h +  PI_range_3h 
#                + IR_mean_6h + IR_sd_6h + IR_med_6h +  IR_range_6h 
#                + PI_mean_6h + PI_sd_6h + PI_med_6h +  PI_range_6h 
#                + IR_mean_12h + IR_sd_12h + IR_med_12h + IR_range_12h 
#                + PI_mean_12h + PI_sd_12h + PI_med_12h + PI_range_12h 
#                + IR_mean_24h + IR_sd_24h + IR_med_24h + IR_range_24h 
#                + PI_mean_24h + PI_sd_24h + PI_med_24h + PI_range_24h 
#                ,data = train, family=binomial(link=logit) )
# summary(model.A)
# 
# test$pred.A <- predict(model.A,newdata = test,type="response")
# summary(test$pred.A)
# test$pred.A.y <- ifelse(test$pred.A >=0.2,1,0)
# table(test$pred.A.y)
# table(test$y,test$pred.A.y)
# 

# # model.A : mean sd med min max
# model.A <- glm(y ~ IR_mean_1h + IR_sd_1h + IR_med_1h + IR_min_1h + IR_max_1h
#                + PI_mean_1h + PI_sd_1h + PI_med_1h + PI_min_1h + PI_max_1h  
#                + IR_mean_3h + IR_sd_3h + IR_med_3h + IR_min_3h + IR_max_3h  
#                + PI_mean_3h + PI_sd_3h + PI_med_3h + PI_min_3h + PI_max_3h  
#                + IR_mean_6h + IR_sd_6h + IR_med_6h + IR_min_6h + IR_max_6h  
#                + PI_mean_6h + PI_sd_6h + PI_med_6h + PI_min_6h + PI_max_6h  
#                + IR_mean_12h + IR_sd_12h + IR_med_12h + IR_min_12h + IR_max_12h 
#                + PI_mean_12h + PI_sd_12h + PI_med_12h + PI_min_12h + PI_max_12h 
#                + IR_mean_24h + IR_sd_24h + IR_med_24h + IR_min_24h + IR_max_24h 
#                + PI_mean_24h + PI_sd_24h + PI_med_24h + PI_min_24h + PI_max_24h 
#                + IR_mean_48h + IR_sd_48h + IR_med_48h + IR_min_48h + IR_max_48h 
#                + PI_mean_48h + PI_sd_48h + PI_med_48h + PI_min_48h + PI_max_48h 
#                ,data = train, family=binomial(link=logit) )
# summary(model.A)
# 
# test$pred.A <- predict(model.A,newdata = test,type="response")
# summary(test$pred.A)
# test$pred.A.y <- ifelse(test$pred.A >=0.2,1,0)
# table(test$pred.A.y)
# table(test$y,test$pred.A.y)


# model.B : mean sd med min max 48h
model.B <- glm(y ~ IR_mean_1h + IR_sd_1h + IR_med_1h + IR_min_1h + IR_max_1h
               + PI_mean_1h + PI_sd_1h + PI_med_1h + PI_min_1h + PI_max_1h  
               + IR_mean_3h + IR_sd_3h + IR_med_3h + IR_min_3h + IR_max_3h  
               + PI_mean_3h + PI_sd_3h + PI_med_3h + PI_min_3h + PI_max_3h  
               + IR_mean_6h + IR_sd_6h + IR_med_6h + IR_min_6h + IR_max_6h  
               + PI_mean_6h + PI_sd_6h + PI_med_6h + PI_min_6h + PI_max_6h  
               + IR_mean_12h + IR_sd_12h + IR_med_12h + IR_min_12h + IR_max_12h 
               + PI_mean_12h + PI_sd_12h + PI_med_12h + PI_min_12h + PI_max_12h 
               + IR_mean_24h + IR_sd_24h + IR_med_24h + IR_min_24h + IR_max_24h 
               + PI_mean_24h + PI_sd_24h + PI_med_24h + PI_min_24h + PI_max_24h 
               + IR_mean_48h + IR_sd_48h + IR_med_48h + IR_min_48h + IR_max_48h 
               + PI_mean_48h + PI_sd_48h + PI_med_48h + PI_min_48h + PI_max_48h 
               ,data = train, family=binomial(link=logit) )
summary(model.B)

test$pred.B <- predict(model.B,newdata = test,type="response")
summary(test$pred.B)
test$pred.B.y <- ifelse(test$pred.B >=0.2,1,0)
table(test$pred.B.y)
table(test$y,test$pred.B.y)




# 03. variable selection --lasso ------------------------------------------


# split train & test set to 8:2
# set.seed(210608)
# 
# samples <- sample(1:nrow(summary_plates),size=0.8*nrow(summary_plates),replace=F)
# train <- summary_plates[samples,]
# test <- summary_plates[-samples,]

# LASSO - L1 norm

x <- train %>% select(-c(key,'Plate',ranges)) %>% data.matrix()
y <- train$y


# model3 : lambda 0.005
model.lasso <- glmnet(x, y, alpha = 1, family = "binomial",
                       lambda = 0.005)
summary(model.lasso)
coef(model.lasso)

# model3 predict
test$pred.lasso <- predict(model.lasso,newx=x.test, type="response")
summary(test$pred.lasso)

test$pred.lasso.y <- ifelse(test$pred.lasso >=0.1,1,0)

table(test$pred.lasso.y)
table(test$y,test$pred.lasso.y)


# cross-validation 으로 optimal lambda 찾기 -- 오래 걸리고 성능 x.....

# cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# plot(cv.lasso)
# fit.lasso <- glmnet(x, y, alpha = 1, family="binomial", nlambda = 100)
# fit.lasso.cv <- cv.glmnet(x, y, family = "binomial",
#                           nfolds = 10, alpha=1, lambda = fit.lasso$lambda)
# plot(fit.lasso.cv) 
# 
# 
# # model1 : lambda 최소 
# model.lasso <- glmnet(x, y, alpha = 1, family = "binomial",
#                       lambda = fit.lasso.cv$lambda.min)
# 
# summary(model.lasso)
# coef(model.lasso)
# 
# # model1 predict
# 
# test$pred.lasso <- predict(model.lasso,newx=x.test, type="response")
# summary(test$pred.lasso)
# 
# test$pred.lasso.y <- ifelse(test$pred.lasso >=0.1,1,0)
# 
# table(test$pred.lasso.y)
# table(test$y,test$pred.lasso.y)

# # model2 : lambda 1se
# model.lasso2 <- glmnet(x, y, alpha = 1, family = "binomial",
#                       lambda = fit.lasso.cv$lambda.1se)
# 
# summary(model.lasso2)
# coef(model.lasso2)
# 
# # model2 predict
# 
# test$pred.lasso2 <- predict(model.lasso2,newx=x.test, type="response")
# summary(test$pred.lasso2)
# 
# test$pred.lasso2.y <- ifelse(test$pred.lasso2 >=0.1,1,0)
# 
# table(test$pred.lasso2.y)
# table(test$y,test$pred.lasso2.y)


save(train, test, file="../0.Data/N3_RData/model2_data.Rdata")

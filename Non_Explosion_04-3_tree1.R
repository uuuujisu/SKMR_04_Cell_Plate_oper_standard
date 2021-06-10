rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(rpart)) install.packages('rpart'); require(rpart)
if(!require(rpart.plot)) install.packages('rpart.plot'); require(rpart.plot)
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

my.max <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T))

# 01. dataset3 ------------------------------------------------------------

tmp_1h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  filter(before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_1h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_1h = my.max(IR_Plates_sd^2),
         IR_ER_1h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_1h = my.max(IR_Plates_range),
         IR_VE_1h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_1h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_1h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_1h = my.max(PI_Plates_sd^2),
         PI_ER_1h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_1h = my.max(PI_Plates_range),
         PI_VE_1h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_1h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_1h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_1h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_1h = my.max(IR_Plates_max),PI_max_1h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_1h', 'IR_MV_1h' ,'IR_ER_1h' , 'IR_MR_1h' ,'IR_VE_1h' , 'IR_VM_1h' ,
           'PI_EV_1h', 'PI_MV_1h' ,'PI_ER_1h', 'PI_MR_1h' ,'PI_VE_1h' , 'PI_VM_1h' ,
           'IR_mean_1h' ,'PI_mean_1h','IR_max_1h' ,'PI_max_1h')) %>%
  distinct()%>%
  ungroup

tmp_3h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  filter(before_h=="3h"| before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_3h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_3h = my.max(IR_Plates_sd^2),
         IR_ER_3h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_3h = my.max(IR_Plates_range),
         IR_VE_3h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_3h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_3h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_3h = my.max(PI_Plates_sd^2),
         PI_ER_3h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_3h = my.max(PI_Plates_range),
         PI_VE_3h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_3h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_3h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_3h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_3h = my.max(IR_Plates_max),PI_max_3h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_3h', 'IR_MV_3h' ,'IR_ER_3h' , 'IR_MR_3h' ,'IR_VE_3h' , 'IR_VM_3h' ,
           'PI_EV_3h', 'PI_MV_3h' ,'PI_ER_3h', 'PI_MR_3h' ,'PI_VE_3h' , 'PI_VM_3h' ,
           'IR_mean_3h' ,'PI_mean_3h','IR_max_3h' ,'PI_max_3h')) %>%
  distinct()%>%
  ungroup


tmp_6h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  filter(before_h=="6h"| before_h=="3h"| before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_6h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_6h = my.max(IR_Plates_sd^2),
         IR_ER_6h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_6h = my.max(IR_Plates_range),
         IR_VE_6h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_6h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_6h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_6h = my.max(PI_Plates_sd^2),
         PI_ER_6h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_6h = my.max(PI_Plates_range),
         PI_VE_6h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_6h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_6h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_6h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_6h = my.max(IR_Plates_max),PI_max_6h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_6h', 'IR_MV_6h' ,'IR_ER_6h' , 'IR_MR_6h' ,'IR_VE_6h' , 'IR_VM_6h' ,
           'PI_EV_6h', 'PI_MV_6h' ,'PI_ER_6h', 'PI_MR_6h' ,'PI_VE_6h' , 'PI_VM_6h' ,
           'IR_mean_6h' ,'PI_mean_6h','IR_max_6h' ,'PI_max_6h')) %>%
  distinct()%>%
  ungroup


tmp_12h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  filter(before_h=="12h"| before_h=="6h"| before_h=="3h"| before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_12h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_12h = my.max(IR_Plates_sd^2),
         IR_ER_12h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_12h = my.max(IR_Plates_range),
         IR_VE_12h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_12h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_12h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_12h = my.max(PI_Plates_sd^2),
         PI_ER_12h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_12h = my.max(PI_Plates_range),
         PI_VE_12h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_12h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_12h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_12h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_12h = my.max(IR_Plates_max),PI_max_12h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_12h', 'IR_MV_12h' ,'IR_ER_12h' , 'IR_MR_12h' ,'IR_VE_12h' , 'IR_VM_12h' ,
           'PI_EV_12h', 'PI_MV_12h' ,'PI_ER_12h', 'PI_MR_12h' ,'PI_VE_12h' , 'PI_VM_12h' ,
           'IR_mean_12h' ,'PI_mean_12h','IR_max_12h' ,'PI_max_12h')) %>%
  distinct()%>%
  ungroup


tmp_24h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  filter(before_h=="24h"| before_h=="12h"| before_h=="6h"| before_h=="3h"| before_h=="1h") %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_24h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_24h = my.max(IR_Plates_sd^2),
         IR_ER_24h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_24h = my.max(IR_Plates_range),
         IR_VE_24h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_24h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_24h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_24h = my.max(PI_Plates_sd^2),
         PI_ER_24h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_24h = my.max(PI_Plates_range),
         PI_VE_24h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_24h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_24h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_24h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_24h = my.max(IR_Plates_max),PI_max_24h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_24h', 'IR_MV_24h' ,'IR_ER_24h' , 'IR_MR_24h' ,'IR_VE_24h' , 'IR_VM_24h' ,
           'PI_EV_24h', 'PI_MV_24h' ,'PI_ER_24h', 'PI_MR_24h' ,'PI_VE_24h' , 'PI_VM_24h' ,
           'IR_mean_24h' ,'PI_mean_24h','IR_max_24h' ,'PI_max_24h')) %>%
  distinct()%>%
  ungroup


tmp_48h <- summary %>%
  select(c(key,'Time','before_h','IR_Plates_mean','IR_Plates_max','IR_Plates_sd','IR_Plates_range',
           'PI_Plates_mean','PI_Plates_max','PI_Plates_sd','PI_Plates_range')) %>%
  group_by(File_num,Item_No) %>%
  mutate(IR_EV_48h = mean(IR_Plates_sd^2,na.rm=TRUE), IR_MV_48h = my.max(IR_Plates_sd^2),
         IR_ER_48h = mean(IR_Plates_range,na.rm=TRUE), IR_MR_48h = my.max(IR_Plates_range),
         IR_VE_48h = var(IR_Plates_mean,na.rm=TRUE), IR_VM_48h =var(IR_Plates_max,na.rm=TRUE) ) %>%
  mutate(PI_EV_48h = mean(PI_Plates_sd^2,na.rm=TRUE), PI_MV_48h = my.max(PI_Plates_sd^2),
         PI_ER_48h = mean(PI_Plates_range,na.rm=TRUE), PI_MR_48h = my.max(PI_Plates_range),
         PI_VE_48h = var(PI_Plates_mean,na.rm=TRUE), PI_VM_48h =var(PI_Plates_max,na.rm=TRUE) ) %>%
  mutate(IR_mean_48h = mean(IR_Plates_mean,na.rm=TRUE),PI_mean_48h = mean(PI_Plates_mean,na.rm=TRUE),
         IR_max_48h = my.max(IR_Plates_max),PI_max_48h = my.max(PI_Plates_max)) %>%
  select(c(key,'IR_EV_48h', 'IR_MV_48h' ,'IR_ER_48h' , 'IR_MR_48h' ,'IR_VE_48h' , 'IR_VM_48h' ,
           'PI_EV_48h', 'PI_MV_48h' ,'PI_ER_48h', 'PI_MR_48h' ,'PI_VE_48h' , 'PI_VM_48h' ,
           'IR_mean_48h' ,'PI_mean_48h','IR_max_48h' ,'PI_max_48h')) %>%
  distinct()%>%
  ungroup



summary_tree <- tmp_1h %>% 
  full_join(tmp_3h, by = key) %>% 
  full_join(tmp_6h, by = key) %>% 
  full_join(tmp_12h, by = key) %>%
  full_join(tmp_24h, by = key) %>%
  full_join(tmp_48h, by = key) %>% 
  arrange(File_num,Item_No) 

summary_tree$y <- as.factor(summary_tree$y)

# 결측 제거 2,634 -> 2,631
NArow <- which(is.na(summary_tree),arr.ind=TRUE)
summary_tree <- summary_tree[-NArow[,1],]


save(summary_tree, file="../0.Data/N3_RData/summary_tree_data.Rdata")
#load("../0.Data/N3_RData/summary_tree_data.Rdata")



# 02. tree  ------------------------------------------------------------

smrs <- colnames(summary_tree)[5:100]
key <- colnames(summary_tree)[1:4]
EV <- colnames(summary_tree)[grep("EV",colnames(summary_tree))]
MV <- colnames(summary_tree)[grep("MV",colnames(summary_tree))]
ER <- colnames(summary_tree)[grep("ER",colnames(summary_tree))]
MR <- colnames(summary_tree)[grep("MR",colnames(summary_tree))]
VE <- colnames(summary_tree)[grep("VE",colnames(summary_tree))]
VM <- colnames(summary_tree)[grep("VM",colnames(summary_tree))]
mean <- colnames(summary_tree[grep("mean",colnames(summary_tree))])
max <- colnames(summary_tree[grep("max",colnames(summary_tree))])


set.seed(210609)

# samples <- sample(1:nrow(summary_tree),size=0.8*nrow(summary_tree),replace=F)
# train <- summary_tree[samples,-c(1:3)]
# test <- summary_tree[-samples,-c(1:3)]

alldata <- summary_tree[,-c(1:3)]
tree.all <- rpart(y ~ ., data=alldata,method="class")
rpart.plot(tree.all)
pred.all <- predict(tree.all,newdata=alldata, type = "class")
confusionMatrix(pred.all, alldata$y)

subdata1 <- summary_tree[,c('y',EV,VE,mean)]
tree.sub1 <- rpart(y ~ ., data=subdata1)
rpart.plot(tree.sub1)
pred.sub1 <- predict(tree.sub1,newdata=alldata, type = "class")
confusionMatrix(pred.sub1, alldata$y)


# subdata2 <- summary_tree[,c('y',EV,ER,VE,mean)]
# tree.sub2 <- rpart(y ~ ., data=subdata2)
# rpart.plot(tree.sub2)

# subdata3 <- summary_tree[,c('y',EV,MV,VE,VM,mean)]
# tree.sub3 <- rpart(y ~ ., data=subdata3)
# rpart.plot(tree.sub3)
# 
# subdata4 <- summary_tree[,c('y',EV,MV,MR,VE,VM,mean)]
# tree.sub4 <- rpart(y ~ ., data=subdata4)
# rpart.plot(tree.sub4)

subdata5 <- summary_tree[,c('y',EV,VE,mean,max)]
tree.sub5 <- rpart(y ~ ., data=subdata5)
rpart.plot(tree.sub5)
pred.sub5 <- predict(tree.sub5,newdata=alldata, type = "class")
confusionMatrix(pred.sub5, alldata$y)

# subdata6 <- summary_tree[,c('y',EV,ER,VE,mean, max)]
# tree.sub6 <- rpart(y ~ ., data=subdata6)
# rpart.plot(tree.sub6)

# subdata7 <- summary_tree[,c('y',EV,MV,VE,VM,mean,max)]
# tree.sub7 <- rpart(y ~ ., data=subdata7)
# rpart.plot(tree.sub7)
# 
# subdata8 <- summary_tree[,c('y',EV,MV,MR,VE,VM,mean,max)]
# tree.sub8 <- rpart(y ~ ., data=subdata8)
# rpart.plot(tree.sub8)

# all=sub3=sub4=sub7=sub8, sub1=sub2 sub5=sub6 결과같음


summary_tree$pred1 <- pred.sub1
summary_tree$pred2 <- pred.all

tree_yn <- summary_tree %>%
  select(c(key,'pred1','pred2'))

table(tree_yn$y,tree_yn$pred1)
table(tree_yn$y,tree_yn$pred2)

save(tree_yn,file="../0.Data/N3_RData/tree_yn.Rdata")

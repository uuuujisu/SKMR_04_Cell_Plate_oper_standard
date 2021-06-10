rm(list=ls())

# environment -------------------------------------------------------------

#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(anytime)) install.packages('anytime'); require(anytime)
if(!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)

#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ROC curve ---------------------------------------------------------------


# #Rdata load
# load(file="../0.Data/N3_RData/model1_data.Rdata")
# 
# test1 <- test[,c(1:4,35:38)]
# train1 <- train
#   
# #Rdata load
# load(file="../0.Data/N3_RData/model2_data.Rdata")
# 
# test2 <- test[,c(1:5,78:81)]
# train2 <- train

# # ROC curve  
# roc1.B <- roc(test1$y, test1$pred.B)
# plot.roc(roc1.B)
# 
# roc1.L <- roc(test1$y, test1$pred.lasso)
# plot.roc(roc1.L)
# 
# roc2.B <- roc(test2$y, test2$pred.B)
# plot.roc(roc1.B)
# 
# roc2.L <- roc(test2$y, test2$pred.lasso)
# plot.roc(roc1.L)



# alpha, beta error 그래프 확인 ------------------------------------------------

load(file="../0.Data/N3_RData/prep02_data.Rdata")
load(file="../0.Data/N3_RData/tree_yn.Rdata")

table(tree_yn$y,tree_yn$pred1)
table(tree_yn$y,tree_yn$pred2)


tmp <- prep02_df %>%
  left_join(tree_yn, by = c('File_num','Item_No','y_date','y'))


# tree1

# alpha1 <- tmp %>%
#   filter(y==1 & pred1==0)
# #224718/78
# #aa <- unique(beta1$File_num)
# 
# beta1 <- tmp %>%
#   filter(y==0 & pred1==1)
# #20167/7
# 
# True1 <- tmp %>% 
#   filter(y==1 & pred1==1)
# #31691/11

alpha2 <- tree_yn %>%
  filter(y==1 & pred2==0 )

beta2 <- tree_yn %>%
  filter(y==0 & pred2==1)

True2 <- tree_yn %>%
  filter(y==1 & pred2==1)

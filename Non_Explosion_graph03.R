rm(list=ls())

# 00. environment -------------------------------------------------------------

#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if(!require(pROC)) install.packages('pROC'); require(pROC)

#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Rdata load
load(file="../0.Data/N3_RData/model1_data.Rdata")

test1 <- test[,c(1:4,35:38)]
train1 <- train
  
#Rdata load
load(file="../0.Data/N3_RData/model2_data.Rdata")

test2 <- test[,c(1:5,78:81)]
train2 <- train
  
roc1.B <- roc(test1$y, test1$pred.B)
plot.roc(roc1.B)

roc1.L <- roc(test1$y, test1$pred.lasso)
plot.roc(roc1.L)

roc2.B <- roc(test2$y, test2$pred.B)
plot.roc(roc1.B)

roc2.L <- roc(test2$y, test2$pred.lasso)
plot.roc(roc1.L)

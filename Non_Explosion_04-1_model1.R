rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(matrixStats)) install.packages('matrixStats'); require(matrixStats)

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
  mutate(IR_EV_1h = mean(IR_Plates_sd^2), IR_VE_1h = var(IR_Plates_mean)) %>%
  mutate(PI_EV_1h = mean(PI_Plates_sd^2), PI_VE_1h = var(PI_Plates_mean)) %>%
  mutate(IR_mean_1h = mean(IR_Plates_mean)) %>%
  select(c(key,'IR_EV_1h','IR_VE_1h','PI_EV_1h','PI_VE_1h','IR_mean_1h')) %>%
  distinct()




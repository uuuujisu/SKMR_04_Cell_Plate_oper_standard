rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

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


# 01. dataset2 ------------------------------------------------------------

tmp <- tmp_hour %>%
  left_join(prep02_df %>% select(key,'Time','t',IRs, PIs),by=c(key,'Time','t'))

tmp_1h <- NULL
i<-1
for (i in 1:25){
  aa <- tmp %>% filter(before_h=="1h") %>%
    select(key,'Time','t',IRs[i],PIs[i]) %>%
    rename(., IR=IRs[i],PI=PIs[i])
  aa$Plate <- 150+i
  aa <- aa %>% 
    group_by(File_num,Item_No,Plate) %>%
    mutate(IR_mean_1h = mean(IR,na.rm=T),
           IR_sd_1h = sd(IR,na.rm=T),
#           IR_min_1h = min(IR,na.rm=T),
#           IR_max_1h = max(IR,na.rm=T),
#           IR_range_1h = range(IR,na.rm=T),
           PI_mean_1h = mean(PI,na.rm=T),
           PI_sd_1h = sd(PI,na.rm=T)) %>%
#           PI_min_1h = min(PI,na.rm=T),
#           PI_max_1h = max(PI,na.rm=T),
#           PI_range_1h = range(PI,na.rm=T)) %>%
    ungroup

  tmp_1h <- rbind(tmp_1h,bb)
}

tmp_1h <- tmp_1h %>%
  





  
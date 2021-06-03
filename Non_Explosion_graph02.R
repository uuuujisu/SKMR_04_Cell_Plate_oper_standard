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
load(file="../0.Data/N3_RData/info_data.Rdata")
#str(info_df)

# 01. mean 분포 확인 ------------------------------------------------------------

i<-1
for (i in 0:89){
  tmp <- info_df %>% filter(File_num==i)
  Explosion_Cell <- unique(tmp[which(tmp$y=="1"),'Item_No'])
  Non_Explosion_Cell <- unique(tmp[-which(tmp$y=="1"),'Item_No'])

  tmp <- tmp[!is.na(tmp$Z.IR_Plates_mean),]
  
  tmp1 <- tmp %>% filter(Item_No == Explosion_Cell)
  a <- hist(tmp1$Z.IR_Plates_mean,breaks=50)
  
  for (aa in Non_Explosion_Cell){
    tmp2 <- tmp %>% filter(Item_No == aa )
    hist(tmp2$Z.IR_Plates_mean,breaks=50)
  }
  
}


# 전체 시간동안 연소/비연소 분포확인 -----------------------------------------------------


  
  tmp <- info_df[!is.na(info_df$Z.IR_Plates_mean),]
  
  tmp1 <- tmp %>% filter(y=="1")
  hist(tmp1$Z.IR_Plates_mean,breaks=1000)
  
  tmp2 <- tmp %>% filter(y!="1" )
  hist(tmp2$Z.IR_Plates_mean,breaks=1000)
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
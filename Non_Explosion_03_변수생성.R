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
load(file="../0.Data/N3_RData/prep01_data.Rdata")
#summary(prep01_df)


# 01. 시간 범주 생성 -------------------------------------------------------
# 1h/3h/6h/12h/24h/48h


hour_df <- prep01_df 

hour_df$Time <- as.POSIXct(hour_df$Time, origin="1899-12-30", tz="GMT")
hour_df$y_date <- as.POSIXct(hour_df$y_date, origin="1899-12-30", tz="GMT")

Explosion_Time <- unique(hour_df$y_date)


  
  
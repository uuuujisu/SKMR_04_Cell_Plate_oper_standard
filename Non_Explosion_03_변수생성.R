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
#summary(prep01_df)


# 01. 시간 범주 생성 -------------------------------------------------------
# 1h/3h/6h/12h/24h/48h

hour_df <- prep02_df 

hour_df$Time <- as.POSIXct(hour_df$Time, origin="1899-12-30", tz="GMT")
hour_df$y_date <- as.POSIXct(hour_df$y_date, origin="1899-12-30", tz="GMT")

Explosion_Time <- unique(hour_df$y_date)

t1 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-1, unit="hours")
t2 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-3, unit="hours")
t3 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-6, unit="hours")
t4 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-12, unit="hours")
t5 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-24, unit="hours")

hour_df$before_h <- NA
for (i in 1:90){
  hour_df$before_h <- ifelse(hour_df$File_num==i-1 & hour_df$Time >= t1[i] & hour_df$Time <= Explosion_Time[i],"1h",
                             ifelse(hour_df$File_num==i-1 & hour_df$Time >= t2[i] & hour_df$Time < t1[i] ,"3h",
                                    ifelse(hour_df$File_num==i-1 & hour_df$Time >= t3[i] & hour_df$Time < t2[i] ,"6h",
                                           ifelse(hour_df$File_num==i-1 & hour_df$Time >= t4[i] & hour_df$Time < t3[i] ,"12h",
                                                  ifelse(hour_df$File_num==i-1 & hour_df$Time >= t5[i] & hour_df$Time < t4[i] ,"24h",
                                                         ifelse(hour_df$File_num==i-1 & hour_df$Time < t5[i] ,"48h",hour_df$before_h))))))
}
  
#확인
#tmp <- hour_df %>% select(c('File_num','Item_No','Time','y','before_h')) %>% arrange(File_num,Item_No,Time)
#na <- tmp[is.na(tmp$before_df),]
#tmpp <- tmp %>% filter(File_num==0)
#with(tmpp,table(before_h))

save(hour_df, file="../0.Data/N3_RData/hour_data.Rdata")
#rm(list=ls()[-2])


# 02. Plate간의 summary 변수 생성 -------------------------------------------------------









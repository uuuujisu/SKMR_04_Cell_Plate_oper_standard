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


# hour_data save ----------------------------------------------------------


save(hour_df, file="../0.Data/N3_RData/hour_data.Rdata")
#rm(list=ls()[-4])

#Rdata load
load(file="../0.Data/N3_RData/hour_data.Rdata")

#colnames(hour_df)
key <- colnames(hour_df)[1:4]
IRs <- colnames(hour_df)[7:31]
PIs <- colnames(hour_df)[34:58]
Plates <- colnames(hour_df)[71:95]


# 02. 연소 플레이트 info  -------------------------------------------------------

tmp_plates <- hour_df %>% select(c(key,Plates)) %>% filter(y=="1") %>% distinct()
tmp_plates[c(Plates)] <- sapply(tmp_plates[c(Plates)], function(x) {ifelse(x!=0,1,x)})
tmp_plates[c(Plates)] <- sapply(tmp_plates[c(Plates)], as.numeric)

#연소plates갯수
tmp_plates$연소Plates갯수 <- rowSums(tmp_plates[,c(Plates)],na.rm=T)

#연소plates_1 ~ 18 / 연소 플레이트 최대 18개
aa <- matrix(NA,nrow=nrow(tmp_plates),ncol=max(tmp_plates$연소Plates갯수))
for (i in 1:nrow(tmp_plates)){
  a <- paste0("P_",which(tmp_plates[i,c(Plates)]==1)+150)
  for (j in 1:max(tmp_plates$연소Plates갯수)){
    aa[i,j] <- a[j]
  }
}
aa <- as.data.frame(aa)

cname <- c()
  for (i in 1:max(tmp_plates$연소Plates갯수)){
  cname[i] <- paste0("연소Plates_",i)
}
colnames(aa) <- cname

tmp_plates <- cbind(tmp_plates,aa) %>%
  select(c(key,'연소Plates갯수',cname))
tmp_plates$연소Plates_1 <- ifelse(tmp_plates$연소Plates_1 == "P_",NA,tmp_plates$연소Plates_1)

# plate_df <- merge(hour_df, tmp_plates, by = c(key),all.x=TRUE)  %>%
#   arrange(File_num,Item_No,Time)


# 03. summary info  --------------------------------------------------------------

IR_df <- hour_df %>% 
  select(c(key,'Time','before_h',IRs)) %>%
  arrange(File_num,Item_No,Time)
IR_df$IR_Plates_mean <- rowMeans(IR_df[,c(IRs)],na.rm=TRUE)
IR_df$IR_Plates_sd <- rowSds(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_med <- rowMedians(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_min <- rowMins(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_max <- rowMaxs(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_range <- IR_df$IR_Plates_max - IR_df$IR_Plates_min
IR_df <- IR_df %>% select(c(-IRs))

PI_df <- hour_df %>% 
  select(c(key,'Time','before_h',PIs))%>%
  arrange(File_num,Item_No,Time)
PI_df$PI_Plates_mean <- rowMeans(PI_df[,c(PIs)],na.rm=TRUE)
PI_df$PI_Plates_sd <- rowSds(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_med <- rowMedians(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_min <- rowMins(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_max <- rowMaxs(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_range <- PI_df$PI_Plates_max - PI_df$PI_Plates_min
PI_df <- PI_df %>% select(c(-PIs))

tmp_summary <- cbind(IR_df,PI_df %>% select(c(-key,-"Time",-"before_h")))


# info_data save ----------------------------------------------------------

info_df <- merge(tmp_summary,tmp_plates,by=c(key),all.x=TRUE) %>%
  arrange(File_num,Item_No,Time)

save(info_df,tmp_summary,tmp_plates,file="../0.Data/N3_RData/info_data.Rdata")

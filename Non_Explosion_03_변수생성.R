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
#summary(prep02_df)

colnames(prep02_df)
key <- colnames(prep02_df)[1:4]
IRs <- colnames(prep02_df)[8:32]
PIs <- colnames(prep02_df)[35:59]
Plates <- colnames(prep02_df)[72:96]

# 01. 시간 범주 생성 -------------------------------------------------------
# 1h/3h/6h/12h/24h/48h

t_cut <-c()
h <- c(1,3,6,12,24,48)
for (i in 1:6){
  t_cut[i] <- 2881 - 60*h[i]
}

tmp_hour <- prep02_df %>% select(c(key,'Time','t'))
tmp_hour$before_h <- ifelse(tmp_hour$t >= t_cut[1], "1h",
                     ifelse(tmp_hour$t >= t_cut[2], "3h",
                     ifelse(tmp_hour$t >= t_cut[3], "6h",
                     ifelse(tmp_hour$t >= t_cut[4], "12h",
                     ifelse(tmp_hour$t >= t_cut[5], "24h",
                     ifelse(tmp_hour$t >= t_cut[6], "48h", NA))))))
tmp_hour <- tmp_hour %>% arrange(File_num,Item_No,Time) 
  

# 오래걸려서 다른 코드로,,
# tmp_hour <- prep02_df %>% select(c(key,'Time'))
# 
# tmp_hour$Time <- as.POSIXct(tmp_hour$Time, origin="1899-12-30", tz="GMT")
# tmp_hour$y_date <- as.POSIXct(tmp_hour$y_date, origin="1899-12-30", tz="GMT")
# 
# Explosion_Time <- unique(tmp_hour$y_date)
# 
# t1 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-1, unit="hours")
# t2 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-3, unit="hours")
# t3 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-6, unit="hours")
# t4 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-12, unit="hours")
# t5 <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-24, unit="hours")
# 
# tmp_hour$before_h <- NA
# for (i in 1:90){
#   tmp_hour$before_h <- ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time >= t1[i] & tmp_hour$Time <= Explosion_Time[i],"1h",
#                              ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time >= t2[i] & tmp_hour$Time < t1[i] ,"3h",
#                                     ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time >= t3[i] & tmp_hour$Time < t2[i] ,"6h",
#                                            ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time >= t4[i] & tmp_hour$Time < t3[i] ,"12h",
#                                                   ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time >= t5[i] & tmp_hour$Time < t4[i] ,"24h",
#                                                          ifelse(tmp_hour$File_num==i-1 & tmp_hour$Time < t5[i] ,"48h",tmp_hour$before_h))))))
# }
# 
# tmp_hour <- tmp_hour %>% arrange(File_num,Item_No,Time)

#확인
na <- tmp_hour[is.na(tmp_hour$before_h),]
# tmpp <- tmp_hour %>% filter(File_num==0)
# with(tmpp,table(before_h))

# 02. 연소 플레이트 info  -------------------------------------------------------

tmp_plates <- prep02_df %>% select(c(key,Plates)) %>% filter(y=="1") %>% distinct()
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
  select(c(key,'연소Plates갯수',cname)) %>%
  arrange(File_num,Item_No)

tmp_plates$연소Plates_1 <- ifelse(tmp_plates$연소Plates_1 == "P_",NA,tmp_plates$연소Plates_1) 


# 03. summary info --------------------------------------------------------------

IR_df <- prep02_df %>% 
  select(c(key,'Time',IRs)) 
IR_df$IR_Plates_mean <- rowMeans(IR_df[,c(IRs)],na.rm=TRUE)
IR_df$IR_Plates_sd <- rowSds(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_med <- rowMedians(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_min <- rowMins(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_max <- rowMaxs(as.matrix(IR_df[,c(IRs)]),na.rm=TRUE)
IR_df$IR_Plates_range <- IR_df$IR_Plates_max - IR_df$IR_Plates_min
IR_df <- IR_df %>% select(c(-IRs)) %>%
  arrange(File_num,Item_No,Time)

PI_df <- prep02_df %>% 
  select(c(key,'Time',PIs))
PI_df$PI_Plates_mean <- rowMeans(PI_df[,c(PIs)],na.rm=TRUE)
PI_df$PI_Plates_sd <- rowSds(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_med <- rowMedians(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_min <- rowMins(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_max <- rowMaxs(as.matrix(PI_df[,c(PIs)]),na.rm=TRUE)
PI_df$PI_Plates_range <- PI_df$PI_Plates_max - PI_df$PI_Plates_min
PI_df <- PI_df %>% select(c(-PIs)) %>%
  arrange(File_num,Item_No,Time)

tmp_summary <- cbind(IR_df,PI_df %>% select(c(-key,-"Time"))) %>%
  arrange(File_num,Item_No,Time)

#NaN, inf처리필요
#asdf <- IR_df %>% filter(File_num==1,Item_No=="J-J")
tmp_summary[sapply(tmp_summary,is.infinite)] <- NA
tmp_summary[sapply(tmp_summary,is.nan)] <- NA


# 04. scale info ----------------------------------------------------------
#연소기간별/Cell별로 표준화

# tmp_ss <- tmp_summary %>% 
#   group_by(File_num,Item_No) %>% 
#   mutate(Z.IR_Plates_mean=scale(IR_Plates_mean),
#          Z.IR_Plates_sd=scale(IR_Plates_sd),
#          Z.IR_Plates_med=scale(IR_Plates_med),
#          # Z.IR_Plates_min=scale(IR_Plates_min),
#          # Z.IR_Plates_max=scale(IR_Plates_max),
#          Z.IR_Plates_range=scale(IR_Plates_range),
#          Z.PI_Plates_mean=scale(PI_Plates_mean),
#          Z.PI_Plates_sd=scale(PI_Plates_sd),
#          Z.PI_Plates_med=scale(PI_Plates_med),
#          # Z.PI_Plates_min=scale(PI_Plates_min),
#          # Z.PI_Plates_max=scale(PI_Plates_max),
#          Z.PI_Plates_range=scale(PI_Plates_range)) %>%
#   ungroup %>%
#   arrange(File_num,Item_No,Time)

# tmp_sss <- tmp_ss %>% 
#   group_by(File_num,Item_No) %>% 
#   mutate(M0.IR_Plates_mean=IR_Plates_mean-mean(IR_Plates_mean),
#          M0.IR_Plates_med=IR_Plates_med-mean(IR_Plates_med),
#          # M0.IR_Plates_min=IR_Plates_min-mean(IR_Plates_min),
#          # M0.IR_Plates_max=IR_Plates_max-mean(IR_Plates_max),
#          M0.PI_Plates_mean=IR_Plates_mean-mean(PI_Plates_mean),
#          M0.PI_Plates_med=IR_Plates_med-mean(PI_Plates_med)) %>%
#          # M0.PI_Plates_min=IR_Plates_min-mean(PI_Plates_min),
#          # M0.PI_Plates_max=IR_Plates_max-mean(PI_Plates_max)) %>%
#   ungroup %>%
#   arrange(File_num,Item_No,Time)



# info_data save ----------------------------------------------------------
summary <- cbind(tmp_hour,tmp_summary %>% select(-c(key,"Time")) ) %>%
  arrange(File_num,Item_No,Time)

save(tmp_hour,tmp_plates,tmp_summary,summary,file="../0.Data/N3_RData/summary_data.Rdata")

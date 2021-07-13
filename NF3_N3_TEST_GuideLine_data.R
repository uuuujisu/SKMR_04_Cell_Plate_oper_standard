rm(list=ls())

#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(lubridate)) install.packages('lubridate'); require(lubridate)
if(!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if(!require(randomForest)) install.packages('randomForest'); require(randomForest)
if(!require(rpart)) install.packages('rpart'); require(rpart)
if(!require(rpart.plot)) install.packages('rpart.plot'); require(rpart.plot)
if(!require(caret)) install.packages('caret'); require(caret)

#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./R_function.R")

setwd("../Data/N3_Data_1차 연소 6월 검증")

# 1. 데이터 불러오기 -------------------------------------------------------------


#var
var <- c( "File_num"             , "Time"                , "전류"                ,
          "누적전류"             , "Plate_전류_151"      , "Plate_전류_152"      ,
          "Plate_전류_153"       , "Plate_전류_154"      , "Plate_전류_155"      ,
          "Plate_전류_156"       , "Plate_전류_157"      , "Plate_전류_158"      ,
          "Plate_전류_159"       , "Plate_전류_160"      , "Plate_전류_161"      ,
          "Plate_전류_162"       , "Plate_전류_163"      , "Plate_전류_164"      ,
          "Plate_전류_165"       , "Plate_전류_166"      , "Plate_전류_167"      ,
          "Plate_전류_168"       , "Plate_전류_169"      , "Plate_전류_170"      ,
          "Plate_전류_171"       , "Plate_전류_172"      , "Plate_전류_173"      ,
          "Plate_전류_174"       , "Plate_전류_175"      , "Cell액높이_차압식"   ,
          "Cell액높이_접점식_H"  , "Cell액높이_접점식_HH", "Cell액높이_접점식_L" ,
          "Cell액높이_접점식_LL" , "압력차이"            , 
          "Collector_압력_151"   , "Collector_압력_152"  , "Collector_압력_153"  ,
          "Collector_압력_154"   , "Collector_압력_155"  , "Collector_압력_156"  ,
          "Collector_압력_157"   , "Collector_압력_158"  , "Collector_압력_159"  ,
          "Collector_압력_160"   , "Collector_압력_161"  , "Collector_압력_162"  ,
          "Collector_압력_163"   , "Collector_압력_164"  , "Collector_압력_165"  ,
          "Collector_압력_166"   , "Collector_압력_167"  , "Collector_압력_168"  ,
          "Collector_압력_169"   , "Collector_압력_170"  , "Collector_압력_171"  ,
          "Collector_압력_172"   , "Collector_압력_173"  , "Collector_압력_174"  ,
          "Collector_압력_175"   , "음극_압력"           , "양극_압력"           ,
          "CW_CV_Opening"        , "STM_CV_Opening"      , "복도측액_온도"       ,
          "유틸리티측액_온도"    , "양극집합관_온도_171" , "양극집합관_온도_172" ,
          "양극집합관_온도_173"  , "음극집합관_온도"     , "양극ProcessGas_VV"   ,
          "전압")


# N3 Tag 데이터,
N3_Tag_DF <- as.data.frame(read.csv("../N-3 Tag NO.csv", header = TRUE))

# 폴더 list / 폴더 설정
fdr.name <- list.files()


# 연소/비연소 전체 Cell에 대한 min data
File.num <- 0
raw_df <- NULL

for (fdr in 1: length(fdr.name)){
  # 폴더의 fileList / order
  fileList <- list.files(path=fdr.name[fdr])
  num.sort <- as.numeric(gsub("[^\\d]+", "\\1", fileList, perl = TRUE))
  fileList <- fileList[order(num.sort)]
  
  # 폴더 이름 str, 각각 마지막 글자 따서 cell 이름 출력
  Cell <- NULL
  tmpstr <- unlist(strsplit(fdr.name[fdr],split='_', fixed=TRUE))[2]
  tmpstr <- unlist(strsplit(tmpstr,split='-', fixed=TRUE))
  Cell <- paste0(substr(tmpstr[1], 2, 3),"-",gsub('\\d','', tmpstr[2]));rm(tmpstr) 
  
  # cell Tag name
  Tagname_df <-  N3_Tag_DF %>% filter(Item_No==Cell)
  Cell_df <-  NULL    

  # fdr 폴더 내 파일 읽어와서 full data 만드는 loop
  for(i in 1:length(fileList)){
    
    tmp_df <-as.data.frame(read.csv(paste0(fdr.name[fdr],"/",fileList[i]),fill=TRUE,na.strings=c(NA,""),check.names=FALSE))  
    tmp_df <- cbind(File_num=File.num, tmp_df); flush.console;
    
    # Tag name 변수명 변경하는 loop
    for (j in 3:ncol(tmp_df)) {
      names(tmp_df)[j] <- return_CName_chr(Tagname_df,names(tmp_df)[j])
    }
    names(tmp_df)[2] <- "Time"
    
    #tibble package
    tmp_df <- tmp_df[,c(var)] %>% 
      add_column("Item_No" = Cell, .before = 'Time')
    
    Cell_df <- rbind(Cell_df,tmp_df)
    File.num <- File.num + 1;
  }   
  
  raw_df <- rbind(raw_df,Cell_df)
  
}

#정렬
raw_df <- raw_df %>% arrange(File_num,Time)
# 변수 생성
IRs <- colnames(raw_df)[6:30]
PIs <- colnames(raw_df)[37:61]
#전류 load
raw_df$전류load<-round(rowSums(raw_df[,c(IRs)], na.rm = TRUE),digit=-2)
#양극집합관온도
raw_df$양극집합관_온도 <- rowMeans(raw_df[,c('양극집합관_온도_171','양극집합관_온도_172','양극집합관_온도_173')],na.rm=TRUE)
#PI 평균
raw_df$PI_Plates_mean <- rowMeans(raw_df[,c(PIs)],na.rm=TRUE)

#setwd("../N3_Data_1차 연소 6월 검증_R_data")
#save(raw_df,file="raw_df.Rdata")
#load(file="raw_df.Rdata")

# 02. Test Dataset 만들기 ----------------------------------------------------

raw_df$Time <- as.POSIXct(raw_df$Time, origin="1899-12-30", tz="GMT")
time1 <- raw_df$Time[1]

# t
test_df <- raw_df[,c(1:3,68:70,74,76)] %>%
  group_by(File_num) %>%
  mutate(t=row_number()) %>%
  ungroup

tw_df <- NULL
for (j in 1:5035){
  t_df <- test_df %>%
    filter( t > 10*(j-1) & t <= 60 + 10*(j-1)) %>%
    mutate(Time_group = j, 
           start_time = time1 + minutes(10*(j-1)),
           end_time = time1 + minutes(60 + 10*(j-1))) 
  t_df <- t_df %>%
    group_by(File_num) %>%
    mutate(전류load_1h = my.mode(전류load)) %>%
    mutate(양극집합관_온도_171_평균_1h = mean(양극집합관_온도_171,na.rm=TRUE), 양극집합관_온도_171_SD_1h = sd(양극집합관_온도_171,na.rm=TRUE),
                    양극집합관_온도_171_RANGE_1h = my.range(양극집합관_온도_171)) %>%
    mutate(양극집합관_온도_172_평균_1h = mean(양극집합관_온도_172,na.rm=TRUE), 양극집합관_온도_172_SD_1h = sd(양극집합관_온도_172,na.rm=TRUE),
                    양극집합관_온도_172_RANGE_1h = my.range(양극집합관_온도_172)) %>%
    mutate(양극집합관_온도_173_평균_1h = mean(양극집합관_온도_173,na.rm=TRUE), 양극집합관_온도_173_SD_1h = sd(양극집합관_온도_173,na.rm=TRUE),
                    양극집합관_온도_173_RANGE_1h = my.range(양극집합관_온도_173)) %>%
    mutate(PI_평균_1h = mean(PI_Plates_mean,na.rm=TRUE)) %>%
    select(File_num,Item_No,Time_group,start_time,end_time,전류load_1h,
           양극집합관_온도_171_SD_1h, 양극집합관_온도_171_RANGE_1h,
           양극집합관_온도_172_SD_1h, 양극집합관_온도_172_RANGE_1h,
           양극집합관_온도_173_SD_1h, 양극집합관_온도_173_RANGE_1h) %>%
    distinct()
  tw_df <- rbind(tw_df, t_df)
}

tw_df <- tw_df %>% arrange(File_num,Time_group)

# save(tw_df, test_df, file="tw_df.Rdata")
# load(file="tw_df.Rdata")
# save(tw_df, test_df, file="tw_df_171-173.Rdata")



# 03. Prediction ----------------------------------------------------------

#연소 data 불러오기
N3_Explosion_df <- read.csv(file = "../N-3 4~6월 연소 현황 정리.csv") %>% select(Item_No, 연소발생일)
N3_Explosion_df$연소발생일 <- as.POSIXct(N3_Explosion_df$연소발생일, origin="1899-12-30", tz="GMT")

#전류 load 9500 이상 가이드라인 예측값
load_high <- tw_df[tw_df$전류load_1h>=9500,]
load_high$pred_y <- ifelse(load_high$양극집합관_온도_173_SD_1h>=0.54, 1,
                           ifelse(load_high$양극집합관_온도_172_RANGE_1h >= 1.2 & 
                                    load_high$양극집합관_온도_171_SD_1h <0.26, 1,0))
# load_high$pred_y <- ifelse(load_high$전류load_1h < 10500 & (load_high$양극집합관_온도_SD_1h>=0.37 | load_high$양극집합관_온도_RANGE_1h>=1.1) ,1,
#                            ifelse(load_high$전류load_1h >= 10500 & load_high$양극집합관_온도_RANGE_1h>=1.1 & load_high$PI_평균_1h >=125,1,0))

#연소로 예측한 시점
pred_y1 <- load_high[load_high$pred_y==1,] %>% arrange(File_num,Time_group)

#시간 포맷
pred_y1$start_time <- as.POSIXct(pred_y1$start_time, origin="1899-12-30", tz="GMT")
pred_y1$end_time <- as.POSIXct(pred_y1$end_time, origin="1899-12-30", tz="GMT")
raw_df$Time <- as.POSIXct(raw_df$Time, origin="1899-12-30", tz="GMT")

#Cell별로 그래프 확인할 구간 설정
date_df <- pred_y1 %>% 
  group_by(Item_No) %>%
  mutate(date1 = ymd_hms(min(start_time)-hours(12)),
         date2 = ymd_hms(max(end_time)+hours(12))) %>%
  select(File_num, Item_No, date1, date2) %>%
  distinct()

#raw data에 필요한 변수와 확인구간 합치기
raw_date_df <- raw_df %>%
  inner_join(date_df, by=c("File_num","Item_No")) %>%
  filter(date1 < Time & date2 > Time ) %>%
  select(File_num, Item_No, Time, 전류load, 
         양극집합관_온도_171, 양극집합관_온도_172,  양극집합관_온도_173 ,
         PI_Plates_mean,
         date1, date2)

# 04. Explosion -----------------------------------------------------------

N3_67 <- N3_Explosion_df %>% filter(month(연소발생일)==6 | month(연소발생일)==7) %>%
  mutate(date1 = ymd_hms(연소발생일) - hours(12), date2 = ymd_hms(연소발생일) + hours(2))

raw_Exp_df <- raw_df %>% 
  select(File_num, Item_No, Time, 전류load, 
         양극집합관_온도_171, 양극집합관_온도_172,  양극집합관_온도_173 , PI_Plates_mean) %>% 
  inner_join(N3_67, by = c("Item_No"))

save(raw_date_df,pred_y1, N3_Explosion_df,raw_Exp_df, file="dataforgraph.Rdata")
load(file="dataforgraph.Rdata")


# Plot prediction --------------------------------------------------------------------

cell_name <- unique(raw_date_df$Item_No)
for (i in cell_name){
  tmp_df <- raw_date_df %>% filter(Item_No==i)
  p <- ggplot(tmp_df, aes(x=Time, y=전류load)) + 
    coord_cartesian(ylim = c(0, max(tmp_df$전류load))) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==i), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = N3_Explosion_df, aes(xintercept=연소발생일), color="yellow") +
    geom_vline(data = N3_Explosion_df %>% filter(Item_No==i) , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(i," 셀의 전류 load"))
  
  a <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_171)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==i), aes(xintercept=end_time), color="blue") +
    geom_vline(data = N3_Explosion_df, aes(xintercept=연소발생일), color="grey") +
    geom_vline(data = N3_Explosion_df %>% filter(Item_No==i) , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(i," 셀의 양극집합관 온도 171"))
  
  b <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_172)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==i), aes(xintercept=end_time), color="blue") +
    geom_vline(data = N3_Explosion_df, aes(xintercept=연소발생일), color="grey") +
    geom_vline(data = N3_Explosion_df %>% filter(Item_No==i) , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(i," 셀의 양극집합관 온도 172"))
  
  c <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_173)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==i), aes(xintercept=end_time), color="blue") +
    geom_vline(data = N3_Explosion_df, aes(xintercept=연소발생일), color="grey") +
    geom_vline(data = N3_Explosion_df %>% filter(Item_No==i) , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(i," 셀의 양극집합관 온도 173"))
  
  d <- ggplot(tmp_df, aes(x=Time, y=PI_Plates_mean)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==i), aes(xintercept=end_time), color="blue") +
    geom_vline(data = N3_Explosion_df, aes(xintercept=연소발생일), color="grey") +
    geom_vline(data = N3_Explosion_df %>% filter(Item_No==i) , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(i," 셀의 Collector_PI"))
  
  print(p)
  print(a)
  print(b)
  print(c)
  print(d)
  
}


# plot explosion ----------------------------------------------------------


Exp_date <- unique(raw_Exp_df$연소발생일)
for(i in Exp_date){
  tmp_df <- raw_Exp_df %>% filter(연소발생일==i) %>% filter(Time >= date1 & Time <= date2)
  Explosion_Cell <- unique(tmp_df$Item_No)
  
  p <- ggplot(tmp_df, aes(x=Time, y=전류load)) + 
    coord_cartesian(ylim = c(0, max(tmp_df$전류load))) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==Explosion_Cell), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = tmp_df , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(Explosion_Cell," 셀의 전류 load"))
  
  
  a <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_171)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==Explosion_Cell), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = tmp_df , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(Explosion_Cell," 셀의 양극집합관 온도 171"))
  
  b <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_172)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==Explosion_Cell), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = tmp_df , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(Explosion_Cell," 셀의 양극집합관 온도 172"))
  
  c <- ggplot(tmp_df, aes(x=Time, y=양극집합관_온도_173)) +
    coord_cartesian(ylim = c(40,60)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==Explosion_Cell), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = tmp_df , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(Explosion_Cell," 셀의 양극집합관 온도 173"))
  
  d <- ggplot(tmp_df, aes(x=Time, y=PI_Plates_mean)) + geom_line() + theme_bw() +
    geom_vline(data = pred_y1 %>% filter(Item_No==Explosion_Cell), aes(xintercept=end_time), color="blue") + 
    geom_vline(data = tmp_df , aes(xintercept=연소발생일), color="red") +
    ggtitle(paste0(Explosion_Cell," 셀의 Collector_PI"))
  
  print(p)
  print(a)
  print(b)
  print(c)
  print(d)
}  


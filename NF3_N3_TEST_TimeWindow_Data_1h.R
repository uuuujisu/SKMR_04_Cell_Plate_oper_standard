rm(list=ls())

#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

if(!require(matrixStats)) install.packages('matrixStats'); require(matrixStats)
if(!require(lubridate)) install.packages('lubridate'); require(lubridate)

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

raw_df <- raw_df %>% arrange(File_num,Time)

IRs <- colnames(raw_df)[6:30]
PIs <- colnames(raw_df)[37:61]


# 변수 생성

#IR summary 
raw_df$IR_Plates_mean <- rowMeans(raw_df[,c(IRs)],na.rm=TRUE)
raw_df$IR_Plates_sd <- rowSds(as.matrix(raw_df[,c(IRs)]),na.rm=TRUE)
raw_df$IR_Plates_med <- rowMedians(as.matrix(raw_df[,c(IRs)]),na.rm=TRUE)
raw_df$IR_Plates_min <- rowMins(as.matrix(raw_df[,c(IRs)]),na.rm=TRUE)
raw_df$IR_Plates_max <- rowMaxs(as.matrix(raw_df[,c(IRs)]),na.rm=TRUE)
raw_df$IR_Plates_range <- raw_df$IR_Plates_max - raw_df$IR_Plates_min

#PI summary
raw_df$PI_Plates_mean <- rowMeans(raw_df[,c(PIs)],na.rm=TRUE)
raw_df$PI_Plates_sd <- rowSds(as.matrix(raw_df[,c(PIs)]),na.rm=TRUE)
raw_df$PI_Plates_med <- rowMedians(as.matrix(raw_df[,c(PIs)]),na.rm=TRUE)
raw_df$PI_Plates_min <- rowMins(as.matrix(raw_df[,c(PIs)]),na.rm=TRUE)
raw_df$PI_Plates_max <- rowMaxs(as.matrix(raw_df[,c(PIs)]),na.rm=TRUE)
raw_df$PI_Plates_range <- raw_df$PI_Plates_max - raw_df$PI_Plates_min

#전류 load
raw_df$전류load <- round(raw_df$IR_Plates_mean,digit=-2)

# 측액온도차이
raw_df$측액온도차이 <- abs(raw_df$유틸리티측액_온도 - raw_df$복도측액_온도)

# 양극집합관온도
raw_df$양극집합관_온도 <- rowMeans(raw_df[,c('양극집합관_온도_171','양극집합관_온도_172','양극집합관_온도_173')],na.rm=TRUE)

setwd("../N3_Data_1차 연소 6월 검증_R_data")
# save(raw_df,IRs,PIs,file="raw_df_total.Rdata")
load(file="raw_df_total.Rdata")
# load(file="raw_df.Rdata")


# 02. Test Dataset 만들기 ----------------------------------------------------

raw_df$Time <- as.POSIXct(raw_df$Time, origin="1899-12-30", tz="GMT")
time1 <- raw_df$Time[1]

chrVar <- c('Cell액높이_접점식_H','Cell액높이_접점식_HH','Cell액높이_접점식_L','Cell액높이_접점식_LL', '양극ProcessGas_VV')

# t
test_df <- raw_df %>%
  select(-IRs,-PIs,-chrVar) %>%
  group_by(File_num) %>%
  mutate(t=row_number()) %>%
  ungroup


# 01' select var 1시간 -----------------------------------------------------------


tt <- (max(test_df$t)-60)/10+1
tw_df <- NULL
for (j in 1:tt){
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
           양극집합관_온도_171_평균_1h, 양극집합관_온도_171_SD_1h, 양극집합관_온도_171_RANGE_1h,
           양극집합관_온도_172_평균_1h, 양극집합관_온도_172_SD_1h, 양극집합관_온도_172_RANGE_1h,
           양극집합관_온도_173_평균_1h, 양극집합관_온도_173_SD_1h, 양극집합관_온도_173_RANGE_1h,
           PI_평균_1h) %>%
    distinct()
  tw_df <- rbind(tw_df, t_df)
}
tw_df <- tw_df %>% arrange(File_num,Time_group)
save(tw_df, test_df, file="tw_df.Rdata")


# 02' total var 1시간 ------------------------------------------------------


summaryVar <- c( "File_num" , "Item_No" , "Time_group" , "start_time", "end_time"  ,
                 "전류load_1h"                 , "평균IR_시계열SD_1h"          , "최대IR_시계열SD_1h"          ,
                 "IR플레이트_군집SD_평균_1h"   , "IR플레이트_군집SD_최대_1h"   , "평균IR_시계열RANGE_1h"       , "최대IR_시계열RANGE_1h"       ,
                 "IR플레이트_군집RANGE_평균_1h", "IR플레이트_군집RANGE_최대_1h", "IR_평균_1h"                  , "평균PI_시계열SD_1h"          ,
                 "최대PI_시계열SD_1h"          , "PI플레이트_군집SD_평균_1h"   , "PI플레이트_군집SD_최대_1h"   , "평균PI_시계열RANGE_1h"       ,
                 "최대PI_시계열RANGE_1h"       , "PI플레이트_군집RANGE_평균_1h", "PI플레이트_군집RANGE_최대_1h", "PI_평균_1h"                  ,
                 "Cell액높이_차압식_평균_1h"   , "Cell액높이_차압식_SD_1h"     , "Cell액높이_차압식_RANGE_1h"  , "압력차이_평균_1h"            ,
                 "압력차이_SD_1h"              , "압력차이_RANGE_1h"           , "음극_압력_평균_1h"           , "음극_압력_SD_1h"             ,
                 "음극_압력_RANGE_1h"          , "양극_압력_평균_1h"           , "양극_압력_SD_1h"             , "양극_압력_RANGE_1h"          ,
                 "CW_CV_Opening_평균_1h"       , "CW_CV_Opening_SD_1h"         , "CW_CV_Opening_RANGE_1h"      , "STM_CV_Opening_평균_1h"      ,
                 "STM_CV_Opening_SD_1h"        , "STM_CV_Opening_RANGE_1h"     , "복도측액_온도_평균_1h"       , "복도측액_온도_SD_1h"         ,
                 "복도측액_온도_RANGE_1h"      , "유틸리티측액_온도_평균_1h"   , "유틸리티측액_온도_SD_1h"     , "유틸리티측액_온도_RANGE_1h"  ,
                 "양극집합관_온도_171_평균_1h" , "양극집합관_온도_171_SD_1h"   , "양극집합관_온도_171_RANGE_1h", "양극집합관_온도_172_평균_1h" ,
                 "양극집합관_온도_172_SD_1h"   , "양극집합관_온도_172_RANGE_1h", "양극집합관_온도_173_평균_1h" , "양극집합관_온도_173_SD_1h"   ,
                 "양극집합관_온도_173_RANGE_1h", "양극집합관_온도_평균_1h"     , "양극집합관_온도_SD_1h"       , "양극집합관_온도_RANGE_1h"    ,
                 "음극집합관_온도_평균_1h"     , "음극집합관_온도_SD_1h"       , "음극집합관_온도_RANGE_1h"    , "전압_평균_1h"                ,
                 "전압_SD_1h"                  , "전압_RANGE_1h"     )

tt <- (max(test_df$t)-60)/10+1
tw_df <- NULL
for (j in 1:tt){
  t_df <- test_df %>%
    filter( t > 10*(j-1) & t <= 60 + 10*(j-1)) %>%
    mutate(Time_group = j,
           start_time = time1 + minutes(10*(j-1)),
           end_time = time1 + minutes(60 + 10*(j-1)))
  t_df <- t_df %>%
    group_by(File_num) %>%
    mutate(전류load_1h = my.mode(전류load)) %>%
    mutate(평균IR_시계열SD_1h = sd(IR_Plates_mean,na.rm=TRUE),최대IR_시계열SD_1h = sd(IR_Plates_max,na.rm=TRUE),
           IR플레이트_군집SD_평균_1h = mean(IR_Plates_sd,na.rm=TRUE), IR플레이트_군집SD_최대_1h = my.max(IR_Plates_sd),
           평균IR_시계열RANGE_1h = my.range(IR_Plates_mean),최대IR_시계열RANGE_1h = my.range(IR_Plates_max),
           IR플레이트_군집RANGE_평균_1h = mean(IR_Plates_range,na.rm=TRUE), IR플레이트_군집RANGE_최대_1h = my.max(IR_Plates_range),
           IR_평균_1h = mean(IR_Plates_mean,na.rm=TRUE)) %>%
    mutate(평균PI_시계열SD_1h = sd(PI_Plates_mean,na.rm=TRUE),최대PI_시계열SD_1h = sd(PI_Plates_max,na.rm=TRUE),
           PI플레이트_군집SD_평균_1h = mean(PI_Plates_sd,na.rm=TRUE), PI플레이트_군집SD_최대_1h = my.max(PI_Plates_sd),
           평균PI_시계열RANGE_1h = my.range(PI_Plates_mean),최대PI_시계열RANGE_1h = my.range(PI_Plates_max),
           PI플레이트_군집RANGE_평균_1h = mean(PI_Plates_range,na.rm=TRUE), PI플레이트_군집RANGE_최대_1h = my.max(PI_Plates_range),
           PI_평균_1h = mean(PI_Plates_mean,na.rm=TRUE)) %>%
    mutate(Cell액높이_차압식_평균_1h = mean(Cell액높이_차압식,na.rm=TRUE), Cell액높이_차압식_SD_1h = sd(Cell액높이_차압식,na.rm=TRUE),
           Cell액높이_차압식_RANGE_1h = my.range(Cell액높이_차압식)) %>%
    mutate(압력차이_평균_1h = mean(압력차이,na.rm=TRUE), 압력차이_SD_1h = sd(압력차이,na.rm=TRUE),
           압력차이_RANGE_1h = my.range(압력차이)) %>%
    mutate(음극_압력_평균_1h = mean(음극_압력,na.rm=TRUE), 음극_압력_SD_1h = sd(음극_압력,na.rm=TRUE),
           음극_압력_RANGE_1h = my.range(음극_압력)) %>%
    mutate(양극_압력_평균_1h = mean(양극_압력,na.rm=TRUE), 양극_압력_SD_1h = sd(양극_압력,na.rm=TRUE),
           양극_압력_RANGE_1h = my.range(양극_압력)) %>%
    mutate(CW_CV_Opening_평균_1h = mean(CW_CV_Opening,na.rm=TRUE), CW_CV_Opening_SD_1h = sd(CW_CV_Opening,na.rm=TRUE),
           CW_CV_Opening_RANGE_1h = my.range(CW_CV_Opening)) %>%
    mutate(STM_CV_Opening_평균_1h = mean(STM_CV_Opening,na.rm=TRUE), STM_CV_Opening_SD_1h = sd(STM_CV_Opening,na.rm=TRUE),
           STM_CV_Opening_RANGE_1h = my.range(STM_CV_Opening)) %>%
    mutate(복도측액_온도_평균_1h = mean(복도측액_온도,na.rm=TRUE), 복도측액_온도_SD_1h = sd(복도측액_온도,na.rm=TRUE),
           복도측액_온도_RANGE_1h = my.range(복도측액_온도)) %>%
    mutate(유틸리티측액_온도_평균_1h = mean(유틸리티측액_온도,na.rm=TRUE), 유틸리티측액_온도_SD_1h = sd(유틸리티측액_온도,na.rm=TRUE),
           유틸리티측액_온도_RANGE_1h = my.range(유틸리티측액_온도)) %>%
    mutate(측액온도차이_평균_3h = mean(측액온도차이,na.rm=TRUE), 측액온도차이_SD_3h = sd(측액온도차이,na.rm=TRUE),
           측액온도차이_RANGE_3h = my.range(측액온도차이)) %>%
    mutate(양극집합관_온도_171_평균_1h = mean(양극집합관_온도_171,na.rm=TRUE), 양극집합관_온도_171_SD_1h = sd(양극집합관_온도_171,na.rm=TRUE),
           양극집합관_온도_171_RANGE_1h = my.range(양극집합관_온도_171)) %>%
    mutate(양극집합관_온도_172_평균_1h = mean(양극집합관_온도_172,na.rm=TRUE), 양극집합관_온도_172_SD_1h = sd(양극집합관_온도_172,na.rm=TRUE),
           양극집합관_온도_172_RANGE_1h = my.range(양극집합관_온도_172)) %>%
    mutate(양극집합관_온도_173_평균_1h = mean(양극집합관_온도_173,na.rm=TRUE), 양극집합관_온도_173_SD_1h = sd(양극집합관_온도_173,na.rm=TRUE),
           양극집합관_온도_173_RANGE_1h = my.range(양극집합관_온도_173)) %>%
    mutate(양극집합관_온도_평균_1h = mean(양극집합관_온도,na.rm=TRUE), 양극집합관_온도_SD_1h = sd(양극집합관_온도,na.rm=TRUE),
           양극집합관_온도_RANGE_1h = my.range(양극집합관_온도)) %>%
    mutate(음극집합관_온도_평균_1h = mean(음극집합관_온도,na.rm=TRUE), 음극집합관_온도_SD_1h = sd(음극집합관_온도,na.rm=TRUE),
           음극집합관_온도_RANGE_1h = my.range(음극집합관_온도)) %>%
    mutate(전압_평균_1h = mean(전압,na.rm=TRUE), 전압_SD_1h = sd(전압,na.rm=TRUE), 전압_RANGE_1h = my.range(전압)) %>%
    select(summaryVar) %>%
    distinct()
  tw_df <- rbind(tw_df, t_df)
}

tw_df <- tw_df %>% arrange(File_num,Time_group)
save(tw_df, test_df, file="tw_df_total.Rdata")

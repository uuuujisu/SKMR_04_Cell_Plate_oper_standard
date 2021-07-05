rm(list=ls())

# 1. 데이터 불러오기 -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(lubridate)) install.packages('lubridate'); require(lubridate)

if(!require(matrixStats)) install.packages('matrixStats'); require(matrixStats)
if(!require(glmnet)) install.packages('glmnet'); require(glmnet)
if(!require(caret)) install.packages('caret'); require(caret)
if(!require(randomForest)) install.packages('randomForest'); require(randomForest)

#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")


# raw data ------------------------------------------------------------


#directory - DATA
setwd("../0.Data")

# N3 연소발생여부 데이터
N3_Explosion_DF <- as.data.frame(read.csv("N3 20년 연소 Cell_Collector NO_210604_수정_유지수.csv", header = TRUE))
N3_Explosion_DF <- as.data.frame(sapply(N3_Explosion_DF, function(x) {ifelse(is.na(x)|x==3,0,x)}))

# N3 Tag 데이터
N3_Tag_DF <- as.data.frame(read.csv("N-3 Tag NO.csv", header = TRUE))

# N3 연소/비연소 분데이터
setwd("./nf3__N3_1차 연소_210621")

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
  File.num <- 0
  
  # fdr 폴더 내 파일 읽어와서 full data 만드는 loop
  for(i in 1:length(fileList)){
    
    tmp_df <-as.data.frame(read.csv(paste0(fdr.name[fdr],"/",fileList[i]),fill=TRUE,na.strings=c(NA,""),check.names=FALSE,
                                    nrows = 2881))  
    tmp_df <- cbind(File_num=File.num, tmp_df); flush.console;
    
    # Tag name 변수명 변경하는 loop
    for (j in 3:ncol(tmp_df)) {
      names(tmp_df)[j] <- return_CName_chr(Tagname_df,names(tmp_df)[j])
    }
    names(tmp_df)[2] <- "Time"
    #tibble package
    tmp_df <- tmp_df %>% 
      add_column("Item_No" = Cell, .before = 'Time')
    Cell_df <- rbind(Cell_df,tmp_df)
    File.num <- File.num + 1;
  }   
  
  raw_df <- rbind(raw_df,Cell_df)
  
}

#  var type - numeric/ factor/ time ----------------------------------------


# Time format 변경
raw_df$Time <- as.POSIXct(raw_df$Time, origin="1899-12-30", tz="GMT")
raw_df$Time <- format(raw_df$Time,format='%Y-%m-%d %H:%M')


# data cleaning -----------------------------------------------------------


# data 양쪽에 빈칸 있는경우 있어서 없애는 function ex) "G-S" != "G-s "
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
raw_df$Item_No <- trim(raw_df$Item_No)
N3_Explosion_DF$Item_No <- trim(N3_Explosion_DF$Item_No)


# cell 이름 소문자를 대문자로 변환
raw_df[,'Item_No'] <- toupper(raw_df[,'Item_No'])
N3_Explosion_DF[,'Item_No'] <- toupper(N3_Explosion_DF[,'Item_No'])

# raw_data ---------------------------------------------------------

colnames(raw_df)
IRs <- colnames(raw_df)[6:30]
PIs <- colnames(raw_df)[38:62]
colnames(N3_Explosion_DF)
Plates <- colnames(N3_Explosion_DF)[6:30]

# total data ----------------------------------------------------------

# plates 전류합 column 생성
raw_df$Plates전류합<-rowSums(raw_df[,c(IRs)], na.rm = TRUE)

# 연소 별 start,endtime column 생성
raw_tmp <- raw_df %>% 
  group_by(File_num,Item_No) %>% 
  mutate(GROUP_starttime = min(Time)) %>%
  mutate(GROUP_endtime = max(Time)) %>%
  ungroup
raw_tmp <- raw_tmp %>%
  mutate(y_date = ymd_hm(GROUP_endtime)) %>% 
  mutate(Time = ymd_hm(Time))

# N3 연소발생여부 data에서 Monel Type filtering
N3_Explosion_tmp <- N3_Explosion_DF %>%
  mutate(y=1, y_date=ymd_hm(연소발생일),액보충시간=ymd_hm(액보충시간)) %>% 
  filter(Cell종류=="Monel") %>% 
  select(-c('연소발생월','연소발생일','Cell종류')) 

# file numbering loop
# cell, y_date 같은 것끼리 붙이기

merge_tmp <- merge(raw_tmp, N3_Explosion_tmp,
                   by= c('Item_No','y_date'), all.x = TRUE) 

merge_tmp[,c(Plates,"y")] <-  sapply(merge_tmp[,c(Plates,"y")], function(x) {ifelse(is.na(x),0,x)}) 

total_df <- merge_tmp  %>%
  select(c('File_num','Item_No','y_date','y',everything())) %>% 
  arrange(File_num,Item_No,Time) %>% 
  distinct()


# 2. 전처리 ---------------------------------------------------------------------

# 01. 연소 시점으로부터 2일 내에 다른 cell에서 연소가 발생한 적 있는 셀 삭제 
# 7,778,700 -> 7,680,778

tmp01 <- total_df %>% select(c('File_num','Item_No','y_date','y')) 
tmp01 <- tmp01[!duplicated(tmp01), ] %>%
  arrange(File_num) %>%
  filter(y=="1") %>%
  mutate(before_Item_No = lag(Item_No), before_y_date = lag(y_date)) %>%
  mutate(err = I(y_date - before_y_date < 48*60 & before_Item_No != Item_No))
#같은cell에서 2일 내에 연소가 발생한 경우는 삭제하지 않음 (J-V 3/14 & 3/15)

tmp01 <- tmp01 %>% filter(err==1)

Err_File_num <- tmp01$File_num
Err_Item_No <- tmp01$before_Item_No

for (i in 1:length(Err_File_num)){
  total_df <- subset(total_df,File_num != Err_File_num[i] 
                     | Item_No != Err_Item_No[i])
}

prep01_df <- total_df %>% 
  arrange(File_num,Item_No,Time)


# Rdata load (용량문제로 error 뜰 시)
# save(prep01_df, file="../NF3_N3_GuideLine/prep01_data.Rdata")
# rm(list=ls())
# load(file="../NF3_N3_GuideLine/prep01_data.Rdata")
# colnames(prep01_df)
# key <- colnames(prep01_df)[1:4]
# IRs <- colnames(prep01_df)[8:32]
# PIs <- colnames(prep01_df)[40:64]
# Plates <- colnames(prep01_df)[81:105]

# 3. 변수생성 --------------------------------------------------------------------

#function
my.range <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T)-min(x, na.rm=T))
my.max <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T))
my.mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 1h/2h/3h/...
t_cut <-c()
for (i in 1:48){
  t_cut[i] <- 2881 - 60*i
}


# t
df1 <-  prep01_df %>%
  arrange(File_num,Item_No,Time) %>%
  group_by(File_num,Item_No) %>%
  mutate(t = row_number()) %>%
  ungroup %>%
  select(c('File_num','Item_No','y_date','y','Time','t',everything()))


# 측액온도차이
df1$측액온도차이 <- df1$유틸리티측액_온도 - df1$복도측액_온도

# 양극집합관온도
df1$양극집합관_온도 <- rowMeans(df1[,c('양극집합관_온도_171','양극집합관_온도_172','양극집합관_온도_173')],na.rm=TRUE)

#IR summary 
df1$IR_Plates_mean <- rowMeans(df1[,c(IRs)],na.rm=TRUE)
df1$IR_Plates_sd <- rowSds(as.matrix(df1[,c(IRs)]),na.rm=TRUE)
df1$IR_Plates_med <- rowMedians(as.matrix(df1[,c(IRs)]),na.rm=TRUE)
df1$IR_Plates_min <- rowMins(as.matrix(df1[,c(IRs)]),na.rm=TRUE)
df1$IR_Plates_max <- rowMaxs(as.matrix(df1[,c(IRs)]),na.rm=TRUE)
df1$IR_Plates_range <- df1$IR_Plates_max - df1$IR_Plates_min

#PI summary
df1$PI_Plates_mean <- rowMeans(df1[,c(PIs)],na.rm=TRUE)
df1$PI_Plates_sd <- rowSds(as.matrix(df1[,c(PIs)]),na.rm=TRUE)
df1$PI_Plates_med <- rowMedians(as.matrix(df1[,c(PIs)]),na.rm=TRUE)
df1$PI_Plates_min <- rowMins(as.matrix(df1[,c(PIs)]),na.rm=TRUE)
df1$PI_Plates_max <- rowMaxs(as.matrix(df1[,c(PIs)]),na.rm=TRUE)
df1$PI_Plates_range <- df1$PI_Plates_max - df1$PI_Plates_min

#전류 load
df1$전류load <- round(df1$Plates전류합,digit=-2)

#summary(df1$전류load)
#hist(df1$전류load)
str(df1)

#변수삭제
chrVar <- c('Cell액높이_접점식_H','Cell액높이_접점식_HH','Cell액높이_접점식_L','Cell액높이_접점식_LL',
            '액투입라인_VV','양극ProcessGas_VV','GROUP_starttime','GROUP_endtime','액보충시간')
dltVar <- c('양극집합관_온도_171','양극집합관_온도_172','양극집합관_온도_173','Plates전류합', '전류', '누적전류')

df <- df1 %>% select(-c(IRs,PIs,Plates,chrVar,dltVar)) %>%
  arrange(File_num,Item_No,Time)

#NaN, inf처리필요
df[sapply(df,is.infinite)] <- NA
df[sapply(df,is.nan)] <- NA

str(df)

# 1시간 summary 변수 생성

df_1h <- df %>%
  filter(t >= t_cut[1]) %>%
  group_by(File_num,Item_No) %>%
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
  mutate(양극집합관_온도_평균_1h = mean(양극집합관_온도,na.rm=TRUE), 양극집합관_온도_SD_1h = sd(양극집합관_온도,na.rm=TRUE),
         양극집합관_온도_RANGE_1h = my.range(양극집합관_온도)) %>%
  mutate(음극집합관_온도_평균_1h = mean(음극집합관_온도,na.rm=TRUE), 음극집합관_온도_SD_1h = sd(음극집합관_온도,na.rm=TRUE),
         음극집합관_온도_RANGE_1h = my.range(음극집합관_온도)) %>%
  mutate(전압_평균_1h = mean(전압,na.rm=TRUE), 전압_SD_1h = sd(전압,na.rm=TRUE), 전압_RANGE_1h = my.range(전압)) %>%
  ungroup
df_1h <- df_1h[,c(1:4,32:83)] %>% arrange(File_num,Item_No) %>% distinct()


# n=2,285 
df_1h[sapply(df_1h,is.infinite)] <- NA
df_1h[sapply(df_1h,is.nan)] <- NA
NArow <- which(is.na(df_1h),arr.ind=TRUE)
df_1h <- df_1h[-NArow[,1],]

df_1h$y <- as.factor(df_1h$y)


# save(df,df_1h, file="../NF3_N3_GuideLine/df.Rdata")
load(file="../NF3_N3_GuideLine/df.Rdata")

# 4. 모델링 ------------------------------------------------------------------
set.seed(0628)

#Dataset - Range, std

yn <- df_1h[,c(1:4)]
DF <- df_1h[,-c(1:3)]
rng <- grep("RANGE", colnames(DF))
stdDF <- DF[,-c(rng)]
std <- grep("SD", colnames(DF))
rngDF <- DF[,-c(std)]

# 변수 중요도 ##################

forest <- randomForest(y~., data=DF)
plot_1h <- varImpPlot(forest)

# MODEL A ##################

tree <- rpart(y ~ ., data=DF,method="class")
rpart.plot(tree)
pred <- predict(tree,newdata=DF, type = "class")
confusionMatrix(pred, DF$y)

yn$pred.all <- pred

# MODEL B ##################
selDF <- stdDF[,c(1,3:12,18,22,25,30)]
my.control<-rpart.control(xval=10, minsplit=25)
tree <- rpart(y ~ ., data=selDF,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=selDF, type = "class")
confusionMatrix(pred, selDF$y)

yn$pred.sel <- pred

# MODEL C ##################
sel2DF <- rngDF[,c(1,3:12,18,22,25,30)]
my.control<-rpart.control(xval=10, minsplit=25)
tree <- rpart(y ~ ., data=sel2DF,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=sel2DF, type = "class")
confusionMatrix(pred, sel2DF$y)

yn$pred.sel2 <- pred

# RECALL ##################

recall <- ifelse(yn$pred.all==1 |  yn$pred.sel ==1 | yn$pred.sel2 ==1 ,1,0)
sum(recall)
table(yn$y,recall)


# 전류 load high  --------------------------------------------------------

hist(df_1h$전류load_1h,breaks=26,main = "전류 load 분포 히스토그램", xlab= "전류 load", ylab="빈도수")

df_1h$전류load그룹_1h[df_1h$전류load_1h >=5500 & df_1h$전류load_1h < 7500 ] = "LOW"
df_1h$전류load그룹_1h[df_1h$전류load_1h >=7500 & df_1h$전류load_1h < 9500 ] = "MID"
df_1h$전류load그룹_1h[df_1h$전류load_1h >=9500 ] = "HIGH"

load_high <- df_1h %>% filter(전류load그룹_1h =="HIGH")

table(load_high$y)

yn_high <- load_high[,c(1:4)]
DF <- load_high[,-c(1:3)]
rng <- grep("RANGE", colnames(DF))
stdDF <- DF[,-c(rng)]
std <- grep("SD", colnames(DF))
rngDF <- DF[,-c(std)]

# MODEL D ##################
selDF <- stdDF[,c(1,5:12,18,22,25,30)]
my.control<-rpart.control(xval=10, minsplit=25)
tree <- rpart(y ~ ., data=selDF,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=selDF, type = "class")
confusionMatrix(pred, selDF$y)

yn_high$pred.sel <- pred

# MODEL E ##################
sel2DF <- rngDF[,c(1,5:12,18,22,25,30)]
my.control<-rpart.control(xval=10, minsplit=20)
tree <- rpart(y ~ ., data=sel2DF,method="class", control= my.control)
rpart.plot(tree)
pred <- predict(tree,newdata=sel2DF, type = "class")
confusionMatrix(pred, sel2DF$y)

yn_high$pred.sel2 <- pred

# RECALL ##################
recall <- ifelse( yn_high$pred.sel ==1 | yn_high$pred.sel2 ==1 ,1,0)
sum(recall)
table(yn_high$y,recall)



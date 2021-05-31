rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(lubridate)) install.packages('lubridate'); require(lubridate)


#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")



# (N3 공장) -------------------------------------------------------------------



# 01. raw data ------------------------------------------------------------


#directory - DATA
setwd("../0.Data")
getwd()

# N3 연소발생여부 데이터
N3_Explosion_DF <- as.data.frame(read.csv("N3 20년 연소 Cell_Collector NO.csv", header = TRUE))
N3_Explosion_DF <- as.data.frame(sapply(N3_Explosion_DF, function(x) {ifelse(is.na(x)|x==3,0,x)}))

# N3 Tag 데이터
N3_Tag_DF <- as.data.frame(read.csv("N-3 Tag NO.csv", header = TRUE))

# N3 연소/비연소 분데이터
setwd("./N3_Data")

# 폴더 list / 폴더 설정
fdr.name <- list.files()

# 연소/비연소 전체 Cell에 대한 min data
File.num <- 0
raw_df <- NULL

# fdr <- 19
# I-U cell 양극ProcessGas_VV tag 오류로 해당변수 삭제
# raw_df <- raw_df[,-65]

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
    
    tmp_df <-as.data.frame(read.csv(paste0(fdr.name[fdr],"/",fileList[i]),fill=TRUE,na.strings=c(NA,""),check.names=FALSE))  
    tmp_df <- cbind(File_num=File.num, tmp_df); flush.console;
    
    # Tag name 변수명 변경하는 loop
    for (j in 3:ncol(tmp_df)) {
      names(tmp_df)[j] <- return_CName_chr(Tagname_df,names(tmp_df)[j])
    }
    names(tmp_df)[2] <- "Time"
    #tibble package
    tmp_df <- tmp_df %>% 
      add_column("Item_No" = Cell, .before = 'Time')
    # I-U cell 양극ProcessGas_VV tag 오류로 해당변수 삭제
    tmp_df <- tmp_df[,-65]
    
    Cell_df <- rbind(Cell_df,tmp_df)
    File.num <- File.num + 1;
  }   

  raw_df <- rbind(raw_df,Cell_df)

}

head(raw_df)


# 02. var type - numeric/ factor/ time ----------------------------------------


# data type numeric / factor
Column_name <- c("Item_No", "Time", "액투입라인_VV", "양극ProcessGas_VV")

#numeric 변수가 character 인 경우 있는지 확인하는 코드 
tmp_numvar <- raw_df[,-which(colnames(raw_df)%in%Column_name)]
str(tmp_numvar) #all numeric

col_err <- c()
row <- c()

for (i in 1:ncol(tmp_numvar)){
  if(is.character(tmp_numvar[,i])==TRUE) {
    cat("character column is ",colnames(tmp_numvar)[i],"\n")
    col_err <- i
    for (j in 1:nrow(tmp_numvar)){
      row[j] <- (str_detect(tmp_numvar[j,i],"\\d") | is.na(tmp_numvar[j,i]) )
    }
    
  }
}
numvar_err <- raw_df[row==FALSE,];rm(tmp_numvar)

#type 확인
str(raw_df)
#raw_df[,-which(colnames(raw_df)%in%Column_name)] <- sapply(raw_df[,-which(colnames(raw_df)%in%Column_name)], as.numeric)

# Time format 변경
raw_df$Time <- as.POSIXct(raw_df$Time, origin="1899-12-30", tz="GMT")
raw_df$Time <- format(raw_df$Time,format='%Y-%m-%d %H:%M')


# 03. data cleaning -----------------------------------------------------------


# data 양쪽에 빈칸 있는경우 있어서 없애는 function ex) "G-S" != "G-s "
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
raw_df$Item_No <- trim(raw_df$Item_No)
N3_Explosion_DF$Item_No <- trim(N3_Explosion_DF$Item_No)


# cell 이름 소문자를 대문자로 변환
raw_df[,'Item_No'] <- toupper(raw_df[,'Item_No'])
N3_Explosion_DF[,'Item_No'] <- toupper(N3_Explosion_DF[,'Item_No'])


# (save1) -------------------------------------------------------------------
save(raw_df,N3_Explosion_DF, file = "../N3_RData/raw_data.Rdata")
rm(list=ls())


# 04. total data ----------------------------------------------------------

dddd <- load(file="../N3_RData/raw_data.Rdata")

# plates 전류합 column 생성
raw_df$Plates전류합<-rowSums(raw_df[,6:30], na.rm = TRUE)

# 연소 별 endtime column 생성
raw_tmp <- raw_df %>% 
  group_by(File_num,Item_No) %>% 
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
merge_tmp[,69:94] <-  sapply(merge_tmp[,69:94], function(x) {ifelse(is.na(x),0,x)}) 

total_df <- merge_tmp  %>%
  select(c('File_num','Item_No','y_date','y',everything())) %>% 
  arrange('File_num','Item_No','Time')

str(total_df)


# (save2) -------------------------------------------------------------------
save(total_df,file = "../N3_RData/total_data.Rdata")
#rm(list=ls())



# 데이터 확인 ------------------------------------------------------------------

load(file="D:/NF3연소 사전감지 과제/4.Cell Plate 운전표준수립/YU_JISU/0.Data/N3_RData/total_data.Rdata")

tmp <- total_df %>% select(c('File_num','Item_No','y_date','y','액보충시간'))
tmp <- tmp[!duplicated(tmp), ] %>%
  arrange(File_num)

sss <-sum(as.numeric(tmp$y))

aaa <- tmp %>%
  group_by(File_num) %>%
  mutate(aa = sum(as.numeric(y))) %>%
  select(c('File_num','aa'))
aaa <- aaa[!duplicated(aaa), ]

tmp <- unique(tmp[which(time_df$y=="1"),c('File_num','Item_No',)])
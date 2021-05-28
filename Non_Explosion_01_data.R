rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
packages <- c("dplyr","tidyr","tidyverse")
#install.packages(packages, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))
if(!require(lubridate)) install.packages('lubridate'); require(lubridate)


#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")



# 01. make total data ---------------------------------------------------------

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
total_df <- NULL
Cell_total <- c()
#fdr <- 11

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
#    i <- 1
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
    Cell_df <- rbind(Cell_df,tmp_df)
    File.num <- File.num+1;
  }   

  Cell_total[fdr] <- Cell
  total_df <- rbind(total_df,Cell_df)

}

head(total_df)


# 02. var type - numeric/ factor/ time ----------------------------------------


# data type numeric / factor
Column_name <- c("Item_No", "Time", "액투입라인_VV", "양극ProcessGas_VV")

#numeric 변수가 character 인 경우 있는지 확인하는 코드 
tmp_numvar <- total_df[,-which(colnames(total_df)%in%Column_name)]
str(tmp_numvar)

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
numvar_err <- total_df[row==FALSE,];rm(tmp_numvar)

#type 확인
total_df[,-which(colnames(total_df)%in%Column_name)] <- sapply(total_df[,-which(colnames(total_df)%in%Column_name)], as.numeric)
str(total_df)

# Time format 변경
total_df$Time <- as.POSIXct(total_df$Time, origin="1899-12-30", tz="GMT")
total_df$Time <- format(total_df$Time,format='%Y-%m-%d %H:%M')



# 03. data cleaning -----------------------------------------------------------


# data 양쪽에 빈칸 있는경우 있어서 없애는 function ex) "G-S" != "G-s "
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
total_df$Item_No <- trim(total_df$Item_No)
N3_Explosion_DF$Item_No <- trim(N3_Explosion_DF$Item_No)


# cell 이름 소문자를 대문자로 변환
total_df[,'Item_No'] <- toupper(total_df[,'Item_No'])
N3_Explosion_DF[,'Item_No'] <- toupper(N3_Explosion_DF[,'Item_No'])



# 04. other variables -----------------------------------------------------------


# plates 전류합 column 생성
total_df$Plates전류합<-rowSums(total_df[,6:30], na.rm = TRUE)


# 연소 별 endtime column 생성
total_df <- total_df %>% 
  group_by(File_num,Item_No) %>% 
  mutate(GROUP_endtime = max(Time)) %>%
  ungroup
total_df$GROUP_endtime <- format(total_df$GROUP_endtime,format='%Y-%m-%d %H:%M')


# 연소 시간 요약
total_tmp <-  total_df %>% 
  group_by(Item_No,File_num) %>%
  select(File_num, Item_No, GROUP_endtime) %>%
  ungroup
total_tmp <- total_tmp[!duplicated(total_tmp), ] %>%
  mutate(Key=ymd_hm(GROUP_endtime)) 

# N3 연소발생여부 data에서 Monel Type filtering
N3_Explosion_DF <- N3_Explosion_DF %>%
  mutate(연소여부="1")
N3_Explosion_DF_tmp <- N3_Explosion_DF %>% 
  filter(Cell종류=="Monel") %>%
  mutate(Key=ymd_hm(연소발생일)) 


# file numbering loop
# cell이 같으면서 폭발 날짜도 같은 것끼리 붙이기

merge_tmp <- merge(total_tmp, N3_Explosion_DF_tmp,
                   by.x = c('Item_No','Key'), by.y = c('Item_No', 'Key'), all.x = TRUE) %>% 
  select(-c('연소발생월','연소발생일','Cell종류','액보충시간'))



# for (i in 1:nrow(total_tmp)) {
#   for (j in 1:nrow(N3_Explosion_DF_tmp)) {
#     if (substr(total_tmp[i,'GROUP_endtime'],1,10) == substr(N3_Explosion_DF_tmp[j,'연소발생일'],1,10)
#         & total_tmp[i,]$Item_No == N3_Explosion_DF_tmp[j,]$Item_No ){
#       N3_Explosion_DF_tmp[j, 'File_num'] <- total_tmp[i,'File_num']
#     }
#   }
# }

#File_num error 확인
#N3_Explosion_DF_tmp[is.na(N3_Explosion_DF_tmp[,'File_num']),]
# 8개 : N3_20년_MinData I-AC 폴더에 다른 data가 들어가있음 --------> 다시 수집
# 2개 : Item_No 소문자 i-AC, j-Y --------> 해결
# 1개 : J-V 3/14 3/15 data 1로 통합되어있음 
# 1개 : G-S 데이터 N-3_RG-110S-1.csv 와 N-3_RG-110S-0.csv data 중복
# 1개 : I-V 폴더가 없음...

## G-H 폴더에 복사본 중복 데이터 있어서 삭제함.
## G-H 폴더에 N-3_RG-110H-1.csv N-3_RG-110H-2.csv 중복 
## G-S 폴더에 1,2 파일 중복

merge_tmp <- merge_tmp[complete.cases(merge_tmp[,c('연소여부')]),]
N3_Explosion_DF_tmp <- N3_Explosion_DF_tmp[complete.cases(N3_Explosion_DF_tmp[ , c('File_num')]),]

# J-V Cell 만 남은 'N3_Explosion_DF_tmp' 와 total_df를 join 함.
# 액보충시간, cell 종류, 연소발생일월, plate 별 연소 여부 column 
Merge_df <- merge(total_df, N3_Explosion_DF_tmp, by = c("File_num", "Item_No")) %>% 
  arrange('File_num')



# dirpath <- "../N3_RData/total_df.Rdata"
# save(total_df, N3_Explosion_DF, file = dirpath);rm(list=ls())
# dddd <- load(file=dirpath)







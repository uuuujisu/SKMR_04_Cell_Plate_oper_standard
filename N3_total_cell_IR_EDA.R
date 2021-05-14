rm(list=ls())


# environment -------------------------------------------------------------


#packages
packages <- c("tidyr","tidyverse","dplyr")
#install.packages(packages, dependencies = TRUE)

lapply(packages, library, character.only = TRUE)



#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./Plot_lib.R")
source("./R_function.R")

return_CName_chr<-function(df1, Tag){
    for (i in 1:NCOL(df1)){
        if (df1[i]==Tag){
            return(names(df1)[i])
        }
    }
}


# data --------------------------------------------------------------------

#directory - DATA
setwd("../0.Data")
getwd()


# N3 연소발생여부 데이터
N3_Explosion_DF <- as.data.frame(read.csv("N3 20년 연소 Cell_Collector NO.csv", header = TRUE))
N3_Explosion_DF <- as.data.frame(sapply(N3_Explosion_DF, function(x) {ifelse(is.na(x),0,x)}))

# N3 Tag 데이터
N3_Tag_DF <- as.data.frame(read.csv("N-3 Tag NO.csv", header = TRUE))

# N3 연소 분데이터
setwd("./N3_20년_Cell_MinData")

# 폴더 list / 폴더 설정
fdr.name <- list.files()


# 연소 전체 Cell에 대한 min data
Explotion_Data_df <-  NULL
File.num <- 0
Explotion_total_Data_df <- NULL
Explosion_Cell_total <- c()

for (fdr in 1: length(fdr.name)) {

    # 폴더의 fileList
    fileList <- list.files(path=fdr.name[fdr])
    cat("fdr.name is ",fdr.name[fdr],"\n")
    
    # 폴더 이름 str, 각각 마지막 글자 따서 cell 이름 출력
    
    tmpstr <- unlist(strsplit(fdr.name[fdr],split='_', fixed=TRUE))[2]
    tmpstr <- unlist(strsplit(tmpstr,split='-', fixed=TRUE))
    Explosion_Cell <- paste0(substr(tmpstr[1], 2, 3),"-",gsub('\\d','', tmpstr[2]));rm(tmpstr) 
    
    # J-v cell Tag name
    Tagname_df <-  N3_Tag_DF %>% filter(Item_No==Explosion_Cell)
    
    # fdr 폴더 내 파일 읽어와서 full data 만드는 loop
    for(i in 1:length(fileList)){
        # i <- 1
        tmp_df <-as.data.frame(read.csv(paste0(fdr.name[fdr],"/",fileList[i]),fill=TRUE,na.strings=c(NA,""),check.names=FALSE))  
        File.num <- File.num+1;
        tmp_df <- cbind(File_num=File.num, tmp_df); flush.console;cat(fdr.name[fdr]," 의",i,"번째 파일\n")
        # Tag name 변수명 변경하는 loop
        for (j in 3:ncol(tmp_df)) {
            names(tmp_df)[j] <- return_CName_chr(Tagname_df,names(tmp_df)[j])
        }
        names(tmp_df)[2] <- "Time"
        #tibble package
        tmp_df <- tmp_df %>% 
            add_column("Item_No" = Explosion_Cell, .before = 'Time') %>% 
            add_column("연소여부" = "연소", .before = 'Time')
        Tmp_DF <- data.frame(File_num = tmp_df$File_num,
                             Item_No = tmp_df$Item_No,
                             연소여부 = tmp_df$연소여부,
                             Time = tmp_df$Time,
                             전류 = tmp_df$전류,
                             Plate_전류_151 = tmp_df$Plate_전류_151,
                             Plate_전류_152 = tmp_df$Plate_전류_152,
                             Plate_전류_153 = tmp_df$Plate_전류_153,
                             Plate_전류_154 = tmp_df$Plate_전류_154,
                             Plate_전류_155 = tmp_df$Plate_전류_155,
                             Plate_전류_156 = tmp_df$Plate_전류_156,
                             Plate_전류_157 = tmp_df$Plate_전류_157,
                             Plate_전류_158 = tmp_df$Plate_전류_158,
                             Plate_전류_159 = tmp_df$Plate_전류_159,
                             Plate_전류_160 = tmp_df$Plate_전류_160,
                             Plate_전류_161 = tmp_df$Plate_전류_161,
                             Plate_전류_162 = tmp_df$Plate_전류_162,
                             Plate_전류_163 = tmp_df$Plate_전류_163,
                             Plate_전류_164 = tmp_df$Plate_전류_164,
                             Plate_전류_165 = tmp_df$Plate_전류_165,
                             Plate_전류_166 = tmp_df$Plate_전류_166,
                             Plate_전류_167 = tmp_df$Plate_전류_167,
                             Plate_전류_168 = tmp_df$Plate_전류_168,
                             Plate_전류_169 = tmp_df$Plate_전류_169,
                             Plate_전류_170 = tmp_df$Plate_전류_170,
                             Plate_전류_171 = tmp_df$Plate_전류_171,
                             Plate_전류_172 = tmp_df$Plate_전류_172,
                             Plate_전류_173 = tmp_df$Plate_전류_173,
                             Plate_전류_174 = tmp_df$Plate_전류_174,
                             Plate_전류_175 = tmp_df$Plate_전류_175,
                             Collector_압력_151 = tmp_df$Collector_압력_151,
                             Collector_압력_152 = tmp_df$Collector_압력_152,
                             Collector_압력_153 = tmp_df$Collector_압력_153,
                             Collector_압력_154 = tmp_df$Collector_압력_154,
                             Collector_압력_155 = tmp_df$Collector_압력_155,
                             Collector_압력_156 = tmp_df$Collector_압력_156,
                             Collector_압력_157 = tmp_df$Collector_압력_157,
                             Collector_압력_158 = tmp_df$Collector_압력_158,
                             Collector_압력_159 = tmp_df$Collector_압력_159,
                             Collector_압력_160 = tmp_df$Collector_압력_160,
                             Collector_압력_161 = tmp_df$Collector_압력_161,
                             Collector_압력_162 = tmp_df$Collector_압력_162,
                             Collector_압력_163 = tmp_df$Collector_압력_163,
                             Collector_압력_164 = tmp_df$Collector_압력_164,
                             Collector_압력_165 = tmp_df$Collector_압력_165,
                             Collector_압력_166 = tmp_df$Collector_압력_166,
                             Collector_압력_167 = tmp_df$Collector_압력_167,
                             Collector_압력_168 = tmp_df$Collector_압력_168,
                             Collector_압력_169 = tmp_df$Collector_압력_169,
                             Collector_압력_170 = tmp_df$Collector_압력_170,
                             Collector_압력_171 = tmp_df$Collector_압력_171,
                             Collector_압력_172 = tmp_df$Collector_압력_172,
                             Collector_압력_173 = tmp_df$Collector_압력_173,
                             Collector_압력_174 = tmp_df$Collector_압력_174,
                             Collector_압력_175 = tmp_df$Collector_압력_175)
        
        
        Explotion_Data_df <- rbind(Explotion_Data_df,Tmp_DF)
        
    }   
    Explosion_Cell_total[fdr] <- Explosion_Cell
#    dirpath <- paste0("../N3_20년_Cell_MinData_explosion/",Explosion_Cell,"_mindata.csv")
#    write.csv(Explotion_Data_df,dirpath)
    
}


# preprocessing -----------------------------------------------------------


for (cell in Explosion_Cell_total[-11]){
    Explotion_Data_df <- read.csv(paste0("../N3_20년_Cell_MinData_explosion/",cell,"_mindata.csv"))
    
    # data type numeric / factor
    Column_name <- c("File_num", "Item_No", "연소여부", "Time") #, "액투입라인_VV", "양극ProcessGas_VV"
    Explotion_Data_df[,-which(colnames(Explotion_Data_df)%in%Column_name)] <- sapply(Explotion_Data_df[,-which(colnames(Explotion_Data_df)%in%Column_name)], as.numeric)
    Explotion_Data_df[,which(colnames(Explotion_Data_df)%in%c("연소여부"))] <- sapply(Explotion_Data_df[,which(colnames(Explotion_Data_df)%in%c("연소여부"))], as.factor) 
    # Explotion_Data_df[,which(colnames(Explotion_Data_df)%in%c("연소여부","액투입라인_VV", "양극ProcessGas_VV"))] <- sapply(Explotion_Data_df[,which(colnames(Explotion_Data_df)%in%c("연소여부","액투입라인_VV", "양극ProcessGas_VV"))], as.factor) 
    
    # plates 전류합 column 생성
    Explotion_Data_df$Plates전류합<-rowSums(Explotion_Data_df[,6:30], na.rm = TRUE)
    # Time format 변경
    Explotion_Data_df$Time <- as.POSIXct(Explotion_Data_df$Time, origin="1899-12-30", tz="GMT")
    Explotion_Data_df$Time <- format(Explotion_Data_df$Time,format='%Y-%m-%d %H:%M')
    
    # 연소 별 starttime, endtime column 생성
    Explotion_Data_df <- Explotion_Data_df %>% 
        group_by(File_num) %>% 
        mutate(GROUP_endtime = max(Time)) %>%
        mutate(GROUP_starttime = min(Time)) %>%
        ungroup
    Explotion_Data_df$GROUP_endtime <- format(Explotion_Data_df$GROUP_endtime,format='%Y-%m-%d %H:%M')
    Explotion_Data_df$GROUP_starttime <- format(Explotion_Data_df$GROUP_starttime,format='%Y-%m-%d %H:%M')
    
    # 연소 시간 요약
    Tmp_df <-  Explotion_Data_df %>% 
        group_by(File_num) %>%
        select(File_num,Item_No, GROUP_starttime, GROUP_endtime) %>%
        arrange(File_num) %>%
        ungroup
    Tmp_df <- Tmp_df[!duplicated(Tmp_df), ]
    
    # N3 연소발생여부 data에서 cell Monel Type filtering
    N3_Explosion_DF_tmp <- N3_Explosion_DF %>% 
        filter(Cell종류=="Monel"&Item_No==cell)
    
    # file numbering loop
    for (i in 1:nrow(Tmp_df)) {
        cat(i, "\n")
        for (j in 1:nrow(N3_Explosion_DF_tmp)) {
            if (substr(Tmp_df[i,'GROUP_endtime'],1,10) == substr(N3_Explosion_DF_tmp[j,'연소발생일'],1,10)) {
                N3_Explosion_DF_tmp[j, 'File_num'] <- Tmp_df[i,'File_num']
            }
        }
    }
    N3_Explosion_DF_tmp <- N3_Explosion_DF_tmp[complete.cases(N3_Explosion_DF_tmp[ , c('File_num')]),]
    
    # J-V Cell 만 남은 'N3_Explosion_DF_tmp' 와 Explotion_Data_df를 join 함.
    # 액보충시간, cell 종류, 연소발생일월, plate 별 연소 여부 column 
    Merge_df <- merge(Explotion_Data_df, N3_Explosion_DF_tmp, by = c("File_num", "Item_No")) %>% 
        arrange(Time)
    
    dirpath <- paste0("../N3_20년_Cell_MinData_explosion/",cell,"_mindata_explosion.csv")
#    write.csv(Merge_df,dirpath)
    
    
    rm(Tmp_df, N3_Explosion_DF_tmp)
}


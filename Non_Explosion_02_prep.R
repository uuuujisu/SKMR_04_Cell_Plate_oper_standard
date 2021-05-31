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

#functions
source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")

#Rdata load
load(file="D:/NF3연소 사전감지 과제/4.Cell Plate 운전표준수립/YU_JISU/0.Data/N3_RData/final_data.Rdata")



# 01. 연소 시점으로부터 2일 내에 다른 cell에서 연소가 발생한 적 있는 셀 삭제 -------------------------
# 7,733,855 ->  7,635,901

tmp01 <- final_df %>% select(c('File_num','Item_No','y_date','y')) 
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
  final_df <- subset(final_df,File_num != Err_File_num[i] 
                     | Item_No != Err_Item_No[i])
}














# 
# time_df <- NULL
# i <- 0
# 
# #for (i in 0:89){
# 
#   time_df <- final_df %>%
#     filter(File_num==i)
# 
#   
#   time_df$Time <- as.POSIXct(time_df$Time, origin="1899-12-30", tz="GMT")
#   time_df$액보충시간 <- as.POSIXct(time_df$액보충시간, origin="1899-12-30", tz="GMT")
#   time_df$y_date <- as.POSIXct(time_df$y_date, origin="1899-12-30", tz="GMT")
# 
#   Explosion_Cell <- unique(time_df[which(time_df$y=="1"),'Item_No'])
#   Explosion_Time <- unique(time_df$y_date)  
# 
#   cat("연소 Cell : " , Explosion_Cell)
#   cat("연소발생일 : ",format(Explosion_Time,format='%Y-%m-%d %H:%M'))
#   
# 
#   
#   
#   p <- ggplot(tmp_df, aes(x = anytime(Time) , y = Plates전류합)) + theme_bw() +
#     geom_line(position = "identity", alpha = 0.2, bins = 50, color='black',size=1) +
#     xlab("Time")+ylab("Plates전류합")+
#     geom_vline(data = tmp_df, aes(xintercept = 액보충시간)) +
#     geom_text( aes(x = 액보충시간, y = 0, label = c("액보충시간")))+
#     geom_vline(data = tmp_df, aes(xintercept = 연소발생일),color="red") 
#   
#   p <- p + coord_cartesian()+ ggtitle(paste0(Explosion_Cell,"_",Explosion_Time,"_전류"))
rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)

#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#functions
source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")

#Rdata load
load(file="../0.Data/N3_RData/prep01_data.Rdata")



# 연소/비연소 비교해보기.. ----------------------------------------------------------



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

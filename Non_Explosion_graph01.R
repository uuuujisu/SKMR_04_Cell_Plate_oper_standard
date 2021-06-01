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

time_df <- NULL
i <- 0

#for (i in 0:89){

#for (j in length(Explosion_Cell))
j<-1

tmp_df <- prep01_df %>% filter(File_num==i) 
Explosion_Cell <- unique(tmp_df$Item_No)[j]
tmp_df <- tmp_df %>% filter(Item_No==Explosion_Cell)

Explosion_Time <- unique(tmp_df$GROUP_endtime)

tmp <- colSums(as.data.frame(lapply(tmp_df[,63:87], as.integer)) ,na.rm = FALSE, dims = 1) 
Explosion_Plate <- names(tmp[which(tmp>0)])

#전체 연소 Plate 확인
Explosion_Plate_total <- NULL
for (j in 1:length(Explosion_Plate)){
  Explosion_Plate_total <- paste(Explosion_Plate_total,Explosion_Plate[j],sep=" ")
}

tmp_df1 <- tmp_df[,c(1:5,31:55,60,56:58,62)]
tmp_df1 <- tmp_df1 %>% gather(Collector_num,Presure,Collector_압력_151:Collector_압력_175)

tmp_df1$Num <- str_split_fixed(tmp_df1$Collector_num, fixed("_"), 3)[, 3]
tmp_df1$Collector_num <- NULL

tmp_df2 <- tmp_df[,c(1:30,56:58,60,62)]
tmp_df2 <- tmp_df2 %>% gather(Plate_num,Current,Plate_전류_151:Plate_전류_175)

tmp_df2$Num <- str_split_fixed(tmp_df2$Plate_num, fixed("_"), 3)[, 3]
tmp_df2$Plate_num <- NULL

tmp_df <- merge(tmp_df1, tmp_df2, by = c("File_num","Item_No","y","Time","전류","Plates전류합",
                                         "액보충시간","y_date","GROUP_endtime","GROUP_starttime", "Num")) %>% 
  arrange(Time);#rm(tmp_df1,tmp_df2)

tmp_df$Plate_num <- paste0("P_",tmp_df$Num)
tmp_df$Collector_num <- paste0("C_",tmp_df$Num)
tmp_df$Num <- NULL

tmp_df$Time <- as.POSIXct(tmp_df$Time, origin="1899-12-30", tz="GMT")

#연소발생 1시간전 상위Rank+최하Rank+연소 Plate 선별 Drawing
Flag_Time <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-1, unit="hours")
Tmp_df1 <- tmp_df %>% 
  filter(Time == Flag_Time) %>% 
  arrange(desc(Current)) %>% 
  add_column(Rank=1:25,.before = 'Plate_num') %>% 
  filter((Rank>=1 & Rank<=5)|Rank==25)
Select_Plate <- Tmp_df1$Plate_num;rm(Tmp_df1)
tmp_df <- tmp_df %>% filter(tmp_df[,'Plate_num'] %in% c(Select_Plate, Explosion_Plate))

min_current <- min(tmp_df$Current, na.rm = T)
max_current <- max(tmp_df$Current, na.rm = T)

min_pressure <- min(tmp_df$Presure, na.rm = T)
max_pressure <- max(tmp_df$Presure, na.rm = T)



q <- ggplot(tmp_df,aes(x=Time,y=Current,group=Plate_num)) + ylim(min_current,max_current) +
  geom_line(aes(color=Plate_num))+xlab("Time")+ylab("전류") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values=color_P) +
  theme(legend.position = 'none') 


q <- q + coord_cartesian()+ ggtitle(paste0(Explosion_Cell,"_",Explosion_Time)) +
  geom_vline(data = tmp_df, aes(xintercept = 액보충시간))+
  geom_text(data = tmp_df, aes(x = 액보충시간, y = min_current, label = c("액보충시간"))) +
  geom_vline(data = tmp_df, aes(xintercept = 연소발생일),color="red") + 
  labs(subtitle =  paste0("연소 플레이트 : ",Explosion_Plate_total,"\n")) 

print(q)

r <- ggplot(tmp_df,aes(x=Time,y=Presure,group=Plate_num)) + ylim(min_pressure,max_pressure) +
  geom_line(aes(color=Plate_num))+xlab("Time")+ylab("압력") +
  guides(color = guide_legend(override.aes = list(size = 5))) +      
  scale_color_manual(values=color_P) +
  theme(legend.position = "none")

r <- r + coord_cartesian() +
  geom_vline(data = tmp_df, aes(xintercept = 액보충시간))+
  geom_text(data = tmp_df, aes(x = 액보충시간, y = min_pressure, label = c("액보충시간")))  +
  geom_vline(data = tmp_df, aes(xintercept = 연소발생일),color="red") 

print(r)








time_df <- NULL
i <- 0

#for (i in 0:89){

  time_df <- prep01_df %>%
    filter(File_num==i)


  time_df$Time <- as.POSIXct(time_df$Time, origin="1899-12-30", tz="GMT")
  time_df$액보충시간 <- as.POSIXct(time_df$액보충시간, origin="1899-12-30", tz="GMT")
  time_df$y_date <- as.POSIXct(time_df$y_date, origin="1899-12-30", tz="GMT")

  Explosion_Cell <- unique(time_df[which(time_df$y=="1"),'Item_No'])
  Explosion_Time <- unique(time_df$y_date)

  cat("연소 Cell : " , Explosion_Cell)
  cat("연소발생일 : ",format(Explosion_Time,format='%Y-%m-%d %H:%M'))




  p <- ggplot(tmp_df, aes(x = anytime(Time) , y = Plates전류합)) + theme_bw() +
    geom_line(position = "identity", alpha = 0.2, bins = 50, color='black',size=1) +
    xlab("Time")+ylab("Plates전류합")+
    geom_vline(data = tmp_df, aes(xintercept = 액보충시간)) +
    geom_text( aes(x = 액보충시간, y = 0, label = c("액보충시간")))+
    geom_vline(data = tmp_df, aes(xintercept = 연소발생일),color="red")

  p <- p + coord_cartesian()+ ggtitle(paste0(Explosion_Cell,"_",Explosion_Time,"_전류"))

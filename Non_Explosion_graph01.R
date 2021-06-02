rm(list=ls())

# 00. environment -------------------------------------------------------------


#packages
if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(tidyr)) install.packages('tidyr'); require(tidyr)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(anytime)) install.packages('anytime'); require(anytime)

if(!require(gtable)) install.packages('gtable'); require(gtable)
if(!require(gridExtra)) install.packages('gridExtra'); require(gridExtra)


#directory - R_code
print(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#functions
source("../R_code/Plot_lib.R")
source("../R_code/R_function.R")

#Rdata load
load(file="../0.Data/N3_RData/prep01_data.Rdata")


# graph color -------------------------------------------------------------

pltte1 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
pie(rep(1,25),col=pltte1)

color_P<- c("P_151"=	pltte1[1],
            "P_152"=	pltte1[2],
            "P_153"=	pltte1[3],
            "P_154"=	pltte1[4],
            "P_155"=	pltte1[5],
            "P_156"=	pltte1[6],
            "P_157"=	pltte1[7],
            "P_158"=	pltte1[8],
            "P_159"=	pltte1[9],
            "P_160"=	pltte1[10],
            "P_161"=	pltte1[11],
            "P_162"=	pltte1[12],
            "P_163"=	pltte1[13],
            "P_164"=	pltte1[14],
            "P_165"=	pltte1[15],
            "P_166"=	pltte1[16],
            "P_167"=	pltte1[17],
            "P_168"=	pltte1[18],
            "P_169"=	pltte1[19],
            "P_170"=	pltte1[20],
            "P_171"=	pltte1[21],
            "P_172"=	pltte1[22],
            "P_173"=	pltte1[23],
            "P_174"=	pltte1[24],
            "P_175"=	pltte1[25])




# 연소/비연소 비교해보기.. ----------------------------------------------------------

setwd("D:/4.Cell Plate 운전표준수립/YU_JISU/0.Data/N3_RData/graph")

time_df <- NULL

for (i in 31:42){

time_df <- prep01_df %>%
  filter(File_num==i)

time_df$Time <- as.POSIXct(time_df$Time, origin="1899-12-30", tz="GMT")
time_df$액보충시간 <- as.POSIXct(time_df$액보충시간, origin="1899-12-30", tz="GMT")
time_df$y_date <- as.POSIXct(time_df$y_date, origin="1899-12-30", tz="GMT")

Explosion_Cell <- unique(time_df[which(time_df$y=="1"),'Item_No'])
Non_Explosion_Cell <- unique(time_df[-which(time_df$y=="1"),'Item_No'])
Explosion_Time <- unique(time_df$y_date)

cat("연소 Cell : " , Explosion_Cell)
cat("연소발생일 : ",format(Explosion_Time,format='%Y-%m-%d %H:%M'))

#연소 Cell 전류합

tmp_df <- time_df %>% filter(Item_No==Explosion_Cell)

p <- ggplot(tmp_df, aes(x = anytime(Time) , y = Plates전류합)) + theme_bw() +
  geom_line(position = "identity", alpha = 0.2, color='black',size=1) +
  xlab("Time")+ylab("Plates전류합")+
  geom_vline(data = tmp_df, aes(xintercept = 액보충시간)) +
  geom_text( aes(x = 액보충시간, y = 0, label = c("액보충시간")))+
  geom_vline(data = tmp_df, aes(xintercept = y_date),color="red")
p <- p + coord_cartesian()+ ggtitle(paste0(Explosion_Cell,"_연소_",Explosion_Time,"_전류"))
#print(p)

#연소Cell 전류/압력

tmp <- colSums(as.data.frame(lapply(tmp_df[,71:95], as.integer)) ,na.rm = FALSE, dims = 1) 
Explosion_Plate <- names(tmp[which(tmp>0)])

#전체 연소 Plate 확인
Explosion_Plate_total <- NULL
for (j in 1:length(Explosion_Plate)){
  Explosion_Plate_total <- paste(Explosion_Plate_total,Explosion_Plate[j],sep=" ")
}

#data transpose
tmp_df1 <- tmp_df[,c(1:6,34:58,67:70)]
tmp_df1 <- tmp_df1 %>% gather(Collector_num,Presure,Collector_압력_151:Collector_압력_175)

tmp_df1$Num <- str_split_fixed(tmp_df1$Collector_num, fixed("_"), 3)[, 3]
tmp_df1$Collector_num <- NULL

tmp_df2 <- tmp_df[,c(1:31,67:70)]
tmp_df2 <- tmp_df2 %>% gather(Plate_num,Current,Plate_전류_151:Plate_전류_175)

tmp_df2$Num <- str_split_fixed(tmp_df2$Plate_num, fixed("_"), 3)[, 3]
tmp_df2$Plate_num <- NULL

tmp_df <- merge(tmp_df1, tmp_df2, by = c("File_num","Item_No","y_date","y","Time","전류","Plates전류합",
                                         "액보충시간","GROUP_starttime","GROUP_endtime","Num")) %>% 
  arrange(Time);rm(tmp_df1,tmp_df2)

tmp_df$Plate_num <- paste0("P_",tmp_df$Num)
tmp_df$Collector_num <- paste0("C_",tmp_df$Num)
tmp_df$Num <- NULL

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
  geom_line(aes(color=Plate_num))+xlab("Time")+ylab("Plate_Current") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(values=color_P) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) 


q <- q + coord_cartesian() + 
  geom_vline(data = tmp_df, aes(xintercept = 액보충시간))+
  geom_text(data = tmp_df, aes(x = 액보충시간, y = min_current, label = c("액보충시간"))) +
  geom_vline(data = tmp_df, aes(xintercept = y_date),color="red") + 
  labs(subtitle =  paste0("연소 플레이트 : ",Explosion_Plate_total,"\n")) 

print(q)

r <- ggplot(tmp_df,aes(x=Time,y=Presure,group=Plate_num)) + ylim(min_pressure,max_pressure) +
  geom_line(aes(color=Plate_num))+xlab("Time")+ylab("Collector_Presure") +
  guides(color = guide_legend(override.aes = list(size = 5))) +      
  scale_color_manual(values=color_P) +
  theme(legend.position = "bottom")

r <- r + coord_cartesian() +
  geom_vline(data = tmp_df, aes(xintercept = 액보충시간))+
  geom_text(data = tmp_df, aes(x = 액보충시간, y = min_pressure, label = c("액보충시간")))  +
  geom_vline(data = tmp_df, aes(xintercept = y_date),color="red") 

print(r)

legend = gtable_filter(ggplot_gtable(ggplot_build(r)), "guide-box")
ss <- grid.arrange(p,legend,
                   q+theme(legend.position='none'),
                   r+theme(legend.position='none'),
                   heights = c(1,0.2,1,1),ncol=1)

print(ss)

ggsave(paste0("./",i,"/연소_",Explosion_Cell,".jpg"),ss,width = 5, height = 10)

#비연소 Cell

for( aa in Non_Explosion_Cell){
  tmp_df <- time_df %>% filter(Item_No==aa)
  
  p <- ggplot(tmp_df, aes(x = anytime(Time) , y = Plates전류합)) + theme_bw() +
    geom_line(position = "identity", alpha = 0.2, color='black',size=1) +
    xlab("Time")+ylab("Plates전류합")+
    geom_vline(data = tmp_df, aes(xintercept = y_date),color="red")
  p <- p + coord_cartesian()+ ggtitle(paste0(aa,"_비연소_",Explosion_Time,"_전류"))

  #비연소Cell 전류/압력
  
  #data transpose
  tmp_df1 <- tmp_df[,c(1:6,34:58,67:70)]
  tmp_df1 <- tmp_df1 %>% gather(Collector_num,Presure,Collector_압력_151:Collector_압력_175)
  
  tmp_df1$Num <- str_split_fixed(tmp_df1$Collector_num, fixed("_"), 3)[, 3]
  tmp_df1$Collector_num <- NULL
  
  tmp_df2 <- tmp_df[,c(1:31,67:70)]
  tmp_df2 <- tmp_df2 %>% gather(Plate_num,Current,Plate_전류_151:Plate_전류_175)
  
  tmp_df2$Num <- str_split_fixed(tmp_df2$Plate_num, fixed("_"), 3)[, 3]
  tmp_df2$Plate_num <- NULL
  
  tmp_df <- merge(tmp_df1, tmp_df2, by = c("File_num","Item_No","y_date","y","Time","전류","Plates전류합",
                                           "액보충시간","GROUP_starttime","GROUP_endtime","Num")) %>% 
    arrange(Time);rm(tmp_df1,tmp_df2)
  
  tmp_df$Plate_num <- paste0("P_",tmp_df$Num)
  tmp_df$Collector_num <- paste0("C_",tmp_df$Num)
  tmp_df$Num <- NULL
  
  #연소발생 1시간전 상위Rank+최하Rank+연소 Plate 선별 Drawing
  Flag_Time <- as.POSIXct(Explosion_Time,origin="1899-12-30",tz="GMT") + as.difftime(-1, unit="hours")
  Tmp_df1 <- tmp_df %>% 
    filter(Time == Flag_Time) %>% 
    arrange(desc(Current)) %>% 
    add_column(Rank=1:25,.before = 'Plate_num') %>% 
    filter((Rank>=1 & Rank<=5)|Rank==25)
  Select_Plate <- Tmp_df1$Plate_num;rm(Tmp_df1)
  tmp_df <- tmp_df %>% filter(tmp_df[,'Plate_num'] %in% c(Select_Plate))
  
  min_current <- min(tmp_df$Current, na.rm = T)
  max_current <- max(tmp_df$Current, na.rm = T)
  
  min_pressure <- min(tmp_df$Presure, na.rm = T)
  max_pressure <- max(tmp_df$Presure, na.rm = T)
  
  
  
  q <- ggplot(tmp_df,aes(x=Time,y=Current,group=Plate_num)) + ylim(min_current,max_current) +
    geom_line(aes(color=Plate_num))+xlab("Time")+ylab("Plate_Current") +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    scale_color_manual(values=color_P) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
  q <- q + coord_cartesian() + 
    geom_vline(data = tmp_df, aes(xintercept = y_date),color="red")

  r <- ggplot(tmp_df,aes(x=Time,y=Presure,group=Plate_num)) + ylim(min_pressure,max_pressure) +
    geom_line(aes(color=Plate_num))+xlab("Time")+ylab("Collector_Presure") +
    guides(color = guide_legend(override.aes = list(size = 5))) +      
    scale_color_manual(values=color_P) +
    theme(legend.position = "bottom")
  r <- r + coord_cartesian() +
    geom_vline(data = tmp_df, aes(xintercept = y_date),color="red") 
  
  legend = gtable_filter(ggplot_gtable(ggplot_build(r)), "guide-box")
  
  ss <- grid.arrange(p,legend,
                     q+theme(legend.position='none'),
                     r+theme(legend.position='none'),
                     heights = c(1,0.2,0.9,1),ncol=1)

  ggsave(paste0("./",i,"/비연소_",aa,".jpg"),ss,width = 5, height = 10)
}

}



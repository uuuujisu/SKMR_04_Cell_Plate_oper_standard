install.packages("dtwclust")
library(dtwclust) 
library(dplyr)

N = 300 

X = list() 
for (i in 1:N) { 
  n = 50  
  x = cumsum(sample(c(-1,1),n,TRUE)) 
  X = append(X, list(x)) 
} 

cluster = tsclust(X, k=12L, distance="dtw_basic", type="hierarchical") 
cl = slot(cluster, "cluster") 
plot(cluster, type="sc")

total_merge_df <- read.csv("D:/NF3연소 사전감지 과제/4.Cell Plate 운전표준수립/YU_JISU/0.Data/N3_20년_Cell_MinData_explosion/total_mindata_explosion.csv") 
total_merge_df <- total_merge_df  %>% arrange(Item_No,연소발생일)

num <- unique(total_merge_df$File_num)
tmp_list <-list()

#for (i in 1:length(num)){
for (i in num){
    tmp_df <- total_merge_df %>% filter(File_num== i)
  tmp_df <- na.omit(tmp_df[,57])
  tmp_list <- append(tmp_list,list(tmp_df))
}

cluster = tsclust(tmp_list, k=12L, distance="dtw_basic", type="hierarchical") 
cl = slot(cluster, "cluster") 
plot(cluster, type="sc")


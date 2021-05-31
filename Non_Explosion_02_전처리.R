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
load(file="../0.Data/N3_RData/total_data.Rdata")



# 01. 연소 시점으로부터 2일 내에 다른 cell에서 연소가 발생한 적 있는 셀 삭제 -------------------------
# 7,733,855 ->  7,635,901

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

#data save
prep01_df <- total_df
save(prep01_df,file = "../0.Data/N3_RData/prep01_data.Rdata")

# 02.  --------------------------------------------------------------------












# environment -------------------------------------------------------------


#packages
packages <- c("dplyr","tidyr","tidyverse")
#install.packages(packages, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))


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


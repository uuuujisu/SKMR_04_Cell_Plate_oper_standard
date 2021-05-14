#####################################################################################################################
#                                                                                                                   #
#                                            EDA분석을 위한 Plot Function                                           #
#                                                                                                                   #
#####################################################################################################################
library(RColorBrewer)
library(ggplot2)

#0. base함수
get_density <- function(data, x, y, n = 100) {
  
  x <- data[,x]
  y <- data[,y]
  
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#1. 산점도
scatter <- function(data, x, y, class = NULL, h = NULL, v = NULL, cex = 0.7, title = "", mode = "point"){
  #1-1. 변수 설정
  class <- deparse(substitute(class))
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  
  #1-2. class 설정
  
  #1-2-1. class 존재
  if(class != "NULL"){
    #Factor값이 아닐경우 Factor로 변경
    if(is.factor(data[,class]) == F){
      data[,class] <- factor( data[,class], levels = sort(unique(data[,class])) )
    }
    
    #Factor값을 이용한 색조정
    color_index <- sort(unique(as.integer(data[,class]))) +3
    color_list <- c(brewer.pal(9,"Set1"))
    color <- color_list[color_index]
    
  }
  
  #1-2-2. class 존재안함
  else{class <- NULL}
  
  #1-3. 모드 설정
  
  if(mode == "density"){
    dens <- get_density(data, x, y)
    cex <- dens
  }
  
  #1-4. Plot
  
  ggplot(data = data, aes_string(x = x, y = y, color = class)) +
    {if(mode == "point") geom_point(size = cex)} +
    {if(mode == "density") geom_point(aes_string(size = cex)) } +
    scale_color_manual(values = color) +
    {if (is.null(h) == F) geom_hline(yintercept = h, col = "red", linetype="dashed")} +
    {if (is.null(v) == F) geom_vline(xintercept = v, col = "red", linetype="dashed")} +
    labs(size = 'DENSITY') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

#2. 히스토그램
histogram <- function(data, x, bins = 10, class = NULL,  h = NULL, v = NULL, alpha = 0.3, binwidth = NULL, title = ""){
  
  #2-1. 변수설정
  class <- deparse(substitute(class))
  x <- deparse(substitute(x))
  
  #2-2. class 설정
  
  #2-2-1. class 존재
  if(class != "NULL"){
    #Factor값이 아닐경우 Factor로 변경
    if(is.factor(data[,class]) == F){
      data[,class] <- factor( data[,class], levels = sort(unique(data[,class])) )
    }
    
    #Factor값을 이용한 색조정
    color_index <- sort(unique(as.integer(data[,class])))
    color_list <- c(brewer.pal(9,"Set1"))
    color <- color_list[color_index]
    
  }
  
  #2-2-2. class 존재안함
  else{class <- NULL}
  
  #2-3. binwidth 변수를 이용한 breaks 변수조정(geom_histogram)
  if(is.null(binwidth) == T){breaks <- NULL}
  else{ breaks <- seq(floor(min(data[,x])), ceiling(max(data[,x])), by = binwidth) }
  
  #2-3. Histogram
  ggplot(data = data, aes_string(x = x, fill = class, color = class)) +
    geom_histogram(position = "identity", bins = bins, alpha = alpha, breaks = breaks ) +
    scale_color_manual(values = color )+
    scale_fill_manual(values = color )+
    {if (is.null(h) == F) geom_hline(yintercept = h, col = "red", linetype="dashed")} +
    {if (is.null(v) == F) geom_vline(xintercept = v, col = "red", linetype="dashed")} +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

# #3. 교차 그래프
#
# pairs <- function(data, class, index = 1, line = NA){
#
#   column_index = list(c("Eff_avg","UOC_avg", "ISC_avg", "PMAX"),
#                       c("FF_avg", "PMPP_avg","IMPP_avg", "PMAX"),
#                       c("UMPP_avg", "TEMP_avg", "광량_avg", "PMAX"),
#                       c("Eff_avg","UOC_avg", "ISC_avg", "FF_avg", "PMPP_avg","IMPP_avg", "UMPP_avg", "TEMP_avg", "광량_avg"),
#                       c("Eff_sd","UOC_sd", "ISC_sd", "PMAX"),
#                       c("FF_sd", "PMPP_sd","IMPP_sd", "PMAX"),
#                       c("UMPP_sd", "TEMP_sd", "광량_sd", "PMAX"),
#                       c("Eff_sd","UOC_sd", "ISC_sd", "FF_sd", "PMPP_sd","IMPP_sd", "UMPP_sd", "TEMP_sd", "광량_sd"))
#
#   class <- deparse(substitute(class))
#   x <- deparse(substitute(x))
#   y <- deparse(substitute(y))
#
#   color <- c("#F8766D", "#619CFF", "#00BA38")
#   color_index <- sort(as.integer(unique(data[,class])))
#
#   ggpairs_scatter <-
#     function(data, mapping) {
#       ggplot(data = data, mapping = mapping) +
#         geom_point(alpha = 0.6, size = 0.3) +
#         scale_color_manual(values = color[color_index])+
#         {if(mapping$y == "PMAX" & is.na(line)==F) geom_hline(yintercept = line, col = "red")}
#     }
#
#   ggpairs_histogram <-
#     function(data, mapping) {
#       ggplot(data = data, mapping = mapping) +
#         geom_histogram(position = "identity", alpha = 0.6, bins = 25) +
#         scale_color_manual(values = color[color_index])+
#         scale_fill_manual(values = color[color_index])+
#         {if(mapping$x == "PMAX" & is.na(line)==F) geom_vline(xintercept = line, col = "red")}
#     }
#
#   ggpairs(data,
#           columns = column_index[[index]],
#           mapping = ggplot2::aes_string(color = class, fill = class),
#           upper = list(continuous = wrap("cor", size = 5)),
#           lower = list(continuous = wrap(ggpairs_scatter)),
#           diag = list(continuous = wrap(ggpairs_histogram)),
#           title = "Pairs Plot")
# }


#4. 산점도 for shiny

scatter_shiny <- function(data, x, y, class = NULL, h = "", v = "", cex = 0.7, title = "", mode = "point"){
  
  #1-1. 변수 설정
  
  #1-2. class 설정
  
  #1-2-1. class 존재
  if(is.null(class) == F){
    #Factor값이 아닐경우 Factor로 변경
    if(is.factor(data[,class]) == F){
      data[,class] <- factor( data[,class], levels = sort(unique(data[,class])) )
    }
    
    #Factor값을 이용한 색조정
    color_index <- sort(unique(as.integer(data[,class])))
    color_list <- c(brewer.pal(9,"Set1"))
    color <- color_list[color_index]
    
  }
  
  #1-2-2. class 존재안함
  else{class <- NULL}
  
  #1-3. 모드 설정
  
  if(mode == "density"){
    dens <- get_density(data, x, y)
    cex <- dens
  }
  
  #1-4. Plot
  
  ggplot(data = data, aes_string(x = x, y = y, color = class)) +
    {if(mode == "point") geom_point(size = cex)} +
    {if(mode == "density") geom_point(aes_string(size = cex)) } +
    scale_color_manual(values = color) +
    {if (h != "") geom_hline(yintercept = h, col = "red", linetype="dashed")} +
    {if (v != "") geom_vline(xintercept = v, col = "red", linetype="dashed")} +
    labs(size = 'DENSITY') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

#5. 히스토그램 for shiny
histogram_shiny <- function(data, x, bins = 10, class = NULL,  h = "", v = "", alpha = 0.3, binwidth = NULL, title = ""){
  
  #2-1. 변수설정
  
  #2-2. class 설정
  
  #2-2-1. class 존재
  if(is.null(class) == F){
    #Factor값이 아닐경우 Factor로 변경
    if(is.factor(data[,class]) == F){
      data[,class] <- factor( data[,class], levels = sort(unique(data[,class])) )
    }
    
    #Factor값을 이용한 색조정
    color_index <- sort(unique(as.integer(data[,class])))
    color_list <- c(brewer.pal(9,"Set1"))
    color <- color_list[color_index]
    
  }
  
  #2-2-2. class 존재안함
  else{class <- NULL}
  
  #2-3. binwidth 변수를 이용한 breaks 변수조정(geom_histogram)
  if(is.null(binwidth) == T){breaks <- NULL}
  else{ breaks <- seq(floor(min(data[,x])), ceiling(max(data[,x])), by = binwidth) }
  
  #2-3. Histogram
  ggplot(data = data, aes_string(x = x, fill = class, col = class)) +
    geom_histogram(position = "identity", bins = bins, alpha = alpha, breaks = breaks ) +
    scale_color_manual(values = color )+
    scale_fill_manual(values = color )+
    {if (h != "") geom_hline(yintercept = h, col = "red", linetype="dashed")} +
    {if (v != "") geom_vline(xintercept = v, col = "red", linetype="dashed")} +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

#6. Violin plot
violin<-function(data, y, class=NULL, title="", h = NULL){
  
  class <- deparse(substitute(class))
  y <- deparse(substitute(y))
  
  #2-2-1. class 존재
  if(class!="NULL"){
    #Factor값이 아닐경우 Factor로 변경
    if(is.factor(data[,class]) == F){
      data[,class] <- factor( data[,class], levels = sort(unique(data[,class])) )
    }
    
    #Factor값을 이용한 색조정
    color_index <- sort(unique(as.integer(data[,class])))
    color_list <- c(brewer.pal(9,"Set1"))
    color <- color_list[color_index]
    
  }
  
  #2-2-2. class 존재안함
  else{class <- 1}
  
  ggplot(data, aes_string(class, y, fill = class))+
    geom_violin(scale = "width", alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA )+
    scale_fill_manual(values = color)+
    scale_size_continuous(range = c(2,4))+
    {if (is.null(h) == FALSE) geom_hline(yintercept = h, col = "red",linetype="dashed")} +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}
var_info_fn <- function(data){
  var_list <- names(data)
  for(i in 1:length(var_list)){
    vari     <- data[, i]
    t.count  <- length(vari)
    m.count  <- sum(is.na(vari))
    m.per    <- paste0(round((m.count/t.count)*100,1),"%")
    u.count  <- length(unique(vari))
    type.var <- class(vari)
    if(is.numeric(vari)){
      box.list <- t(boxplot(data[, i], plot = F)$stats)
      m   <- mean(data[, i], na.rm=T)
      sd  <- sd(data[, i], na.rm=T)
      min <- min(data[, i], na.rm=T)
      q1  <- box.list[2]
      med <- median(data[, i], na.rm=T)
      q3  <- box.list[4]
      max <- max(data[, i], na.rm=T)
      ran <- max-min
      cv  <- sd/m
    } else{
      m <- NA ; sd <- NA ; min <- NA ; q1 <- NA;
      med <- NA; q3 <- NA ; max <- NA ; cv <- NA
    }
    ex <- head(as.character(unique(na.omit(vari))), 3)
    ex <- paste0(ex, collapse = ", ")

    single.info <- data.frame(VARIABLE = var_list[i],
                              COUNT = t.count,
                              MISSING = m.count,
                              MISSING_per = m.per,
                              UNIQUE = u.count,
                              TYPE = type.var, EXAMPLE = ex,
                              MEAN = m,
                              SD = sd,
                              MIN = min,
                              Q1 = q1,
                              MEDIAN = med,
                              Q3 = q3, MAX = max, CV = cv)
    if(i == 1){
      info <- single.info
    } else{
      info <- rbind(info, single.info)
    }
  }
  return(info)
}



return_CName_chr<-function(df1, Tag){
  for (i in 1:NCOL(df1)){
    if (df1[i]==Tag){
      return(names(df1)[i])
    }
  }
}

my.range <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T)-min(x, na.rm=T))
my.max <- function(x) ifelse( all(is.na(x)), NA, max(x, na.rm=T))
my.mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


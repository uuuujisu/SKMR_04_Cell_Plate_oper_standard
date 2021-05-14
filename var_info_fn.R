
require(moments)


mode_fn <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

var_info_fn <- function(data){
  var_list <- names(data)
  for(i in 1:length(var_list)){
    vari     <- data[, i]
    t.count  <- length(vari)
    m.count  <- sum(is.na(vari))
    m.per    <- round((m.count/t.count)*100,2)
    u.count  <- length(unique(na.omit(vari)))
    type.var <- class(vari)[1]
    if(is.numeric(vari)){
      m   <- mean(vari, na.rm=T)
      mod <- mode_fn(vari)
      sd  <- sd(vari, na.rm=T)
      min <- min(vari, na.rm=T)
      q1  <- quantile(vari, 0.25, na.rm = T)
      med <- median(vari, na.rm=T)
      q3  <- quantile(vari, 0.75, na.rm = T)
      max <- max(vari, na.rm=T)
      ran <- max-min
      cv  <- sd/m
      qd  <- IQR(vari, na.rm = T)/2
      kur <- kurtosis(vari, na.rm = T)
      skw <- skewness(vari, na.rm = T)
      } else{
      m <- NA ; sd <- NA ; min <- NA ; q1 <- NA;
      med <- NA; q3 <- NA ; max <- NA ; cv <- NA;
      mod <- NA; qd <- NA; kur <- NA; skw <- NA;
      ran <- NA
    }
    ex <- head(as.character(unique(na.omit(vari))), 3)
    ex <- paste0(ex, collapse = ", ")

    single.info <- data.frame(VARIABLE = var_list[i],
                              COUNT = t.count,
                              MISSING = m.count,
                              MISSING_per = m.per,
                              UNIQUE = u.count,
                              TYPE = type.var, 
                              EXAMPLE = ex,
                              
                              MEAN = m,
                              MEDIAN = med,
                              MODE = mod,
                              Q1 = q1,
                              Q3 = q3,
                              MIN = min,
                              MAX = max, 
                              
                              SD = sd,
                              CV = cv,
                              QD = qd,
                              RANGE = ran,
                              
                              KURTOSIS = kur,
                              SKEWMESS = skw,
                              stringsAsFactors = F)
    if(i == 1){
      info <- single.info
    } else{
      info <- rbind(info, single.info)
    }
  }
  
  rownames(info) <- NULL
  return(info)
}

#some function
to_factor <- function(df){#from Tmisc::unfactor
  id <- sapply(df, is.character)
  df[id] <- lapply(df[id], as.factor)
  df
}
cha_to_fac <- function(df){
  id <- sapply(df,is.character)
  df[id] <- lapply(df[id],as.factor)
  df
}
if_impute <- function(x) {
  if(shapiro.test(x)$p.value > 0.05){
    x <- impute(x,fun=mean)
  }
  else{
    x <- impute(x,fun=median)
  }
  return(x)
}

to_impute <- function(df){#from Tmisc::unfactor
  id <- sapply(df, is.numeric)
  df[id] <- lapply(df[id], FUN=if_impute)
  df[id] <- lapply(df[id], as.numeric)
  df
}
get_num <- function(df){
  id <- sapply(df,is.numeric)
  df[id]
}
get_unum <- function(df){#from Tmisc::unfactor
  id <- sapply(df, is.numeric)
  df[-id]
}
get_fac <- function(df){
  id <- sapply(df,is.factor)
  df[id]
}
get_p <- function(x,fac,paired=FALSE,digits = 3){
  if(shapiro.test(x)$p.value >0.05){
    test<- t.test(x~fac,paired = FALSE)
  }
  else{
    test <- wilcox.test(x~fac,paired = FALSE)
  }
  return(signif(test$p.value,digits = digits))
}

get_stat_num <- function(x,na.omit=TRUE,digits=3){
  library(stringr)
  library(dplyr)
  if(na.omit)
    x <- x[!is.na(x)]
  if(shapiro.test(x)$p.value >0.05){
    m <- signif(mean(x),digits = digits)
    s <- signif(sd(x),digits = digits)
    stat <- str_c(m,"±",s)
  }
  else{
    m <- signif(median(x),digits = digits)
    IQR_1 <- signif(quantile(x)[2],digits = digits)
    IQR_3 <- signif(quantile(x)[4],digits = digits)
    stat <- str_c(m,"(",IQR_1,"-",IQR_3,")")
  }
  return(stat)
}
get_stat_mean <- function(x,na.omit=TRUE,digits=3){
  library(stringr)
  library(dplyr)
  if(na.omit)
    x <- x[!is.na(x)]
  m <- signif(mean(x),digits = digits)
  s <- signif(sd(x),digits = digits)
  stat <- str_c(m,"±",s)
  return(stat)
}
get_stat_median <- function(x,na.omit=TRUE,digits=3){
  library(stringr)
  library(dplyr)
  if(na.omit)
    x <- x[!is.na(x)]
  m <- signif(median(x),digits = digits)
  IQR_1 <- signif(quantile(x)[2],digits = digits)
  IQR_3 <- signif(quantile(x)[4],digits = digits)
  stat <- str_c(m,"(",IQR_1,"-",IQR_3,")")
  return(stat)
}

get_stat_fac <- function(x,fac=fac){
  a <- table(x) %>%
    prop.table()%>%
    as.matrix() %>%
    apply(FUN=signif,digit=3,MARGIN = 2) %>%
    t() %>%
    t()*100 
  a <- as.data.frame(a)
  a$Freq <- as.data.frame.table(table(x)) %>% .[[2]] 
  a <- a %>%
    mutate(total= str_c(Freq,"(",V1,"%",")")) %>%
    select(total)
  
  b_0 <- as.data.frame.table(xtabs(~ x+fac)) %>%
    rename(count=Freq)
  b <- 
    xtabs(~ x+fac) %>%
    prop.table(.,margin = 2) %>%
    as.data.frame.table() %>%
    mutate(Freq= signif(Freq,digit=3)) %>%
    mutate(Freq= Freq*100) %>%
    left_join(b_0,by=c("x","fac")) %>%
    mutate(n_Fre=str_c(count,"(",Freq,"%",")")) %>%
    select(x,fac,n_Fre) %>%
    spread( key = fac, value = n_Fre)
  
  c <- xtabs(~ x +fac)%>%
    chisq.test()
  c <- c$p.value
  
  b$Total <- a[[1]]
  b$p_value <- NA
  b$p_value[1] <- c
  b <- select(b,x,Total,everything())
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
eGFR_fun <- function(sex,scr,age) {
  if (sex == "男"){
    X=min(scr/(88.41*0.9),1)^-0.411
    Y=max(scr/(88.41*0.9),1)^-1.029
    Z=0.993^age
    result <- 141*X*Y*Z 
  }
  else{
    X=min(scr/(88.41*0.7),1)^-0.329
    Y=max(scr/(88.41*0.7),1)^-1.029
    Z=0.993^age
    result <- 141*X*Y*Z*1.018
  }
  return(result)
}

lm_table <- function(y_name, df){
  dat <- df %>%
    dplyr::select(y_name, everything())
  y <- dat[[1]]
  res <- lapply(dat[, -1], function(x) {
    fit <- lm(y ~ x)
    β  <- round(coef(fit), 2)
    CI <- round(confint(fit), 2)
    P <- round(coef(summary(fit))[, 4], 3)
    # Names the columns of CI
    colnames(CI) <- c("Lower", "Higher")
    # Bind columns together as dataset
    table2 <- as.data.frame(cbind(β, CI, P))
    # add brackes and line for later use in table
    table2$b <- "~"
    # order the columns
    table2 <- table2[, c("β", "Lower", "b", "Higher", "P")]
    # Merge all columns in one
    library(tidyr)
    table2 = unite(table2,
                   "95%CI",
                   c(Lower, b, Higher),
                   sep = "",
                   remove = T)
    table2 <- table2[-1, ]
    return(table2)
  })
  res <- do.call(rbind, res)
  return(res)
}
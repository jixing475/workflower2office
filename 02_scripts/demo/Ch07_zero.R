cha_to_fac <- function(df){
  id <- sapply(df,is.character)
  df[id] <- lapply(df[id],as.factor)
  df
}
get_num <- function(df){
  id <- sapply(df,is.numeric)
  df[id]
}
get_fac <- function(df){
  id <- sapply(df,is.factor)
  df[id]
}
get_p <- function(x,fac,na.omit=TRUE,paired=FALSE,digits = 3){
  if(na.omit)
    x <- x[!is.na(x)]
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

get_stat_fac <- function(x,fac=fac){
  a <- table(x) %>%
    prop.table()%>%
    t() %>%
    t()*100
  b <- xtabs(~ x+fac) %>%
    prop.table(.,margin = 2) %>%
    as.data.frame.matrix() *100
  c <- xtabs(~ x +fac)%>%
    chisq.test()
  c <- c$p.value
  
  b$Total <- a
  b$p_value <- NA
  b$p_value[1] <- c
  b <- select(b,Total,everything())
}

#==========================================

#TMAO <- cha_to_fac(TMAO)
ifelse(TMAO$TMAO < median(TMAO$TMAO),"low","high") %>%
  as.factor() -> TMAO$TMAO_group
  fac <- as.factor(TMAO$TMAO_group)
  TMAO %>%
    get_num() %>%
    sapply(get_stat_num) -> a#total summary
  TMAO %>%
    filter(get("TMAO_group") == levels(fac)[1]) %>%
    get_num() %>%
    sapply(get_stat_num) -> b#summary for group_1
  TMAO %>%
    filter(get("TMAO_group") == levels(fac)[2]) %>%
    get_num() %>%
    sapply(get_stat_num) -> c#summary for group_2
  TMAO %>%
    get_num() %>%
    sapply(get_p,fac=fac) -> d#get the p value
  result_num <- rbind(a,b,c,d)
  rownames(result_num) <- c("Total",levels(fac)[1],levels(fac)[2],"p_value")
  
rio::export(t(result_num),"num_stat.xlsx",row.names =TRUE) 

  TMAO %>%
    get_fac() %>%
    select(-get("TMAO_group")) %>%
    lapply(.,get_stat_fac,fac=fac)  %>%
    do.call(rbind,.) -> result_table
p_val <- result_table$p_value
result_table <- round(result_table)
result_table$p_value <- p_val
rio::export(result_table,"fac_stat.xlsx",row.names =TRUE) 



  






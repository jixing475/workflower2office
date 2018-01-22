##
TMAO <- TMAO_1_4_5[!is.na(TMAO_1_4_5$sex),]
fac <- TMAO$sex
##
#TMAO_1_4_5$group <- TMAO$TMAO_group
TMAO %>%
  get_num() %>%
  sapply(get_stat_num) -> a#total summary
TMAO %>%
  filter(get("sex") == levels(fac)[1]) %>%
  get_num() %>%
  sapply(get_stat_num) -> b#summary for group_1
TMAO %>%
  filter(get("sex") == levels(fac)[2]) %>%
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
  select(-get("sex")) %>%
  lapply(.,get_stat_fac,fac=fac)  %>%
  do.call(rbind,.) -> result_table
rio::export(result_table,"fac_stat.xlsx",row.names =TRUE) 

View(t(result_num))
View(result_table)

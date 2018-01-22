
TMAO_1_4_5 <- read_excel("~/Desktop/TMAO_ai/#_02_dplyr_TMAO/TMAO_zhangshaowu_chinese.xlsx")
id_name_tmao <- read_excel("~/Desktop/TMAO_ai/#_02_dplyr_TMAO/id_name_tmao.xlsx")
TMAO_1_4_5 <- dplyr::left_join(TMAO_1_4_5,id_name_tmao,by = c("ID号","姓名"))#combine the table 
TMAO_1_4_5$ECG <- factor(TMAO_1_4_5$ECG)
scr_z <- TMAO_1_4_5$肌酐
sex_z <- TMAO_1_4_5$性别
age_z <- TMAO_1_4_5$年龄
eGFR_Z <- c()
for(i in 1:length(sex_z)){
  eGFR_Z <- c(eGFR_Z,eGFR_fun(sex = sex_z[i],scr = scr_z[i],age = age_z[i]))
}
TMAO_1_4_5$eGFR <- eGFR_Z
TMAO_1_4_5 %>%
  remove_empty_cols() %>% remove_empty_rows() %>% Tmisc::convert_to_NA() %>%#remove empty and convert strings to NA
  mutate(血糖 = str_replace(血糖,"mmol/L","")) %>%
  mutate(TMA = as.numeric(TMA)) %>%
  mutate(血糖 = as.numeric(血糖)) %>%
  to_impute() %>%
  mutate(BMI = 体重/((身高/100)^2)) %>%
  mutate(心胸比 = if_else(心胸比=="正常","正常","异常")) %>%
  mutate(心脏超声状态 = if_else(心脏超声=="正常","正常","异常")) %>%
  #mutate(eGFR=ifelse(性别=="男",141*(min(肌酐/(88.41*0.9),1)^-0.411)*(max(肌酐/(88.41*0.9),1)^-1.029)*(0.993^年龄) ,141*(min(肌酐/(88.41*0.7),1)^-0.329)*(max(肌酐/(88.41*0.7),1)^-1.029)*(0.993^年龄)*1.018)) %>%
  mutate(CKD_stage =cut(eGFR, c(min(eGFR,na.rm=T),15,60,max(eGFR,na.rm=T)), right=FALSE,labels = (5:3)) ) %>%
  separate(血压, c("收缩压","舒张压"),sep = "/") %>%
  mutate(收缩压 = as.numeric(收缩压)) %>%
  mutate(舒张压 = as.numeric(舒张压)) %>%
  separate('胆红素(总/直接/间接)', c("总胆红素","直接胆红素","间接胆红素"),sep = "/") %>%
  mutate(总胆红素 = as.numeric(总胆红素)) %>%
  mutate(直接胆红素 = as.numeric(直接胆红素)) %>%
  mutate(间接胆红素 = as.numeric(间接胆红素)) %>%
  separate(入院日期,c("年","月","日")) %>%
  select(-c(ID号,补充的超声心动图异常和特殊时间,心电图))%>%
  to_factor() -> TMAO_12_34_5
levels(TMAO_12_34_5$CKD_stage) <- c("stage_5","stage_3_4","stage_1_2")






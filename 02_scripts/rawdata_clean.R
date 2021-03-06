#raw data clean 

setwd("~/Desktop/TMAO")
##import libray
library(tidyverse)
library(readxl)
library(Tmisc)
library(Hmisc)
source("02_scripts/some_function.R")

##import data
TMAO_1_4_5 <- read_excel("01_data/20170919_combine.xlsx")
id_name_tmao <- read_excel("01_data/id_name_tmao.xlsx")
TMAO_1_4_5 <-
  dplyr::left_join(TMAO_1_4_5, id_name_tmao, by = c("ID号", "姓名"))#combine the table
TMAO_1_4_5 <- TMAO_1_4_5[!is.na(TMAO_1_4_5$TMAO), ]
TMAO_1_4_5$ECG <- factor(TMAO_1_4_5$ECG)
scr_z <- TMAO_1_4_5$肌酐
sex_z <- TMAO_1_4_5$性别
age_z <- TMAO_1_4_5$年龄
eGFR_Z <- c()
for (i in 1:length(sex_z)) {
  eGFR_Z <-
    c(eGFR_Z, eGFR_fun(
      sex = sex_z[i],
      scr = scr_z[i],
      age = age_z[i]
    ))
}
TMAO_1_4_5$eGFR <- eGFR_Z
TMAO_1_4_5 %>%
  remove_empty_cols() %>% remove_empty_rows() %>% 
  Tmisc::convert_to_NA() %>%#remove empty and convert strings to NA
  mutate(血糖 = str_replace(血糖,"mmol/L","")) %>%
  mutate(TMA = as.numeric(TMA)) %>%
  mutate(血糖 = as.numeric(血糖)) %>%
  mutate(BMI = 体重/((身高/100)^2)) %>%
  mutate(心胸比 = if_else(心胸比 == "正常","正常","异常")) %>%
  mutate(心脏超声状态 = if_else(心脏超声 == "正常","正常","异常")) %>%
  mutate(LVEF_stat =if_else(LVEF <= 0.55, "Abnormal", "Normal"))  %>% 
  #mutate(eGFR=ifelse(性别=="男",141*(min(肌酐/(88.41*0.9),1)^-0.411)*(max(肌酐/(88.41*0.9),1)^-1.029)*(0.993^年龄) ,141*(min(肌酐/(88.41*0.7),1)^-0.329)*(max(肌酐/(88.41*0.7),1)^-1.029)*(0.993^年龄)*1.018)) %>%
  mutate(CKD_stage = ifelse(eGFR <= 15,"stage_5","stage_1-4") ) %>%
  separate(血压, c("收缩压","舒张压"),sep = "/") %>%
  mutate(收缩压 = as.numeric(收缩压)) %>%
  mutate(舒张压 = as.numeric(舒张压)) %>%
  separate('胆红素(总/直接/间接)', c("总胆红素","直接胆红素","间接胆红素"),sep = "/") %>%
  mutate(总胆红素 = as.numeric(总胆红素)) %>%
  mutate(直接胆红素 = as.numeric(直接胆红素)) %>%
  mutate(间接胆红素 = as.numeric(间接胆红素)) %>%
  separate(入院日期,c("年","月","日")) %>%
  select(-c(ID号, 补充的超声心动图异常和特殊时间, 心电图)) %>%
  to_factor() -> TMAO_1_4_5

##recode column: include rename colname and type of column
TMAO_1_4_5 %>%
  dplyr::select(-c(姓名, 年, 月, 日, 心脏超声, TMA, 左旋肉碱)) %>%
  rename(
    hypertension =  高血压,
    height =  身高,
    weight =  体重,
    age =  年龄,
    diabetes =  糖尿病,
    sex =  性别,
    smoke =  吸烟,
    drink =  饮酒,
    SBP =  收缩压,
    DBP =  舒张压,
    heart_infarction =  心梗,
    Cerebrovascular_disease = 脑血管病,
    heart_disease =  心脏病,
    creatinine =  肌酐,
    NYHA =  心功能分级,
    albumin =  白蛋白,
    uric_acid =  尿酸,
    total_bilirubin =  总胆红素,
    direct_bilirubin =  直接胆红素,
    indirect_bilirubin =  间接胆红素,
    Cardiothoracic_ratio =  心胸比,
    blood_sugar =  血糖,
    C_reactive_protein = C反应蛋白,
    white_cell =  白细胞,
    Hemoglobin =  血色素,
    heart_ultrasound =  心脏超声状态,
    touxi =  透析) %>% 
  to_impute() -> TMAO_1_4_5


TMAO_1_4_5 <-
  TMAO_1_4_5 %>% dplyr::select(
    -c(xray::anomalies(.)$variables$Variable[1:9])
  ) 
  

## missing value
#sapply(TMAO_1_4_5, function(x)  length(x)-sum(is.na(x)))
#gg_na(TMAO_1_4_5)

##xray explore
#xray::anomalies(TMAO_1_4_5)
#xray::distributions(TMAO_1_4_5) 

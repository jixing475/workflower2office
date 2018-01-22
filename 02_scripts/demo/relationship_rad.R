
##所有变量的相关系数
result <- correlation(dataset = "TMAO_1_4_5", vars = c("age", "LVEF",  "SBP", "DBP", 
                                                       "creatinine", "BUN", "albumin", "uric_acid", 
                                                       "total_bilirubin", "CHOL", "TG", "LDL", "VLDL", "HDL", 
                                                       "blood_sugar", "C_reactive_protein", "white_cell", 
                                                       "Hemoglobin", "eGFR", "BMI", "TMAO"), method = "pearson")
summary(result, covar = TRUE)
plot(result, n = 1000)


##去除stain 药
TMAO_1_4_5 %>%
  filter(stain == "无") -> TMAO_stain
result <- correlation(dataset = "TMAO_stain", vars = c("age", "LVEF",  "SBP", "DBP", "creatinine", "BUN", "albumin", "uric_acid",  "CHOL", "TG", "LDL", "VLDL", "HDL", "blood_sugar", "C_reactive_protein", "white_cell", "Hemoglobin", "eGFR", "BMI", "TMAO"), method = "spearman")
summary(result, covar = TRUE)
plot(result, n = 1000)


##ckd_5期
TMAO_1_4_5 %>%
  filter(CKD_stage == "stage_5") -> TMAO_stage_5
result <- correlation(dataset = "TMAO_stage_5", vars = c("age", "LVEF",  "SBP", "DBP", "creatinine", "BUN", "albumin", "uric_acid",  "CHOL", "TG", "LDL", "VLDL", "HDL", "blood_sugar", "C_reactive_protein", "white_cell", "Hemoglobin", "eGFR", "BMI", "TMAO"), method = "spearman")
summary(result, covar = TRUE)
plot(result, n = 1000)

correlation()

## 回归分析
result <- regress(dataset = "TMAO_1_4_5", rvar = "TMAO", evar = "Cardiothoracic_ration")
summary(result, sum_check = c("rmse", "sumsquares", "vif", "confint"))

#=================和有序因子的相关分析======
#Polyserial Correlation 相关性
TMAO_1_4_5 %>%
  select(Cardiothoracic_ratio,VLDL,blood_sugar,C_reactive_protein) %>%
  as.data.frame() -> cor_data
polycor::hetcor(cor_data)

ID号	高血压	姓名	年龄	LVEF	补充的超声心动图异常和特殊时间	糖尿病	性别	吸烟	身高	饮酒	体重	血压	心梗	胸围	脑血管病	腰围	BMI	心脏病	入院日期	心脏超声	肌酐	BUN	心功能分级	白蛋白	尿酸	Aspirin	胆红素(总/直接/间接)	ACE/ARB	CHOL	stain	TG	Beta-blocker	LDL	心胸比	VLDL	HDL	ECG	心电图	血糖	C反应蛋白	白细胞	血色素




#age,LVEF,SBP,DBP,BUN,albumin,uric_acid,total_bilirubin,direct_bilirubin,indirect_bilirubin,CHOL,TG,LDL,VLDL,HDL,,blood_sugar,C_reactive_protein,white_cell,Hemoglobin,TMAO,eGFR,BMI

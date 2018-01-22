library(dplyr)
dat <- select(TMAO_1_4_5,one_of("TMAO","age","sex","HDL","CHOL","Hemoglobin","eGFR","Cardiothoracic_ratio","heart_ultrasound"))
gg_na(dat)
dat$heart_ultrasound <- factor(dat$heart_ultrasound,levels = c("正常","异常"))
dat$Cardiothoracic_ratio <- factor(dat$Cardiothoracic_ratio,levels = c("正常","异常"))
dat$sex <- factor(dat$sex,levels = c("男","女"))

colnames(dat) <- c("TMAO","Age(yr)","Female","HDL (mM)","Cholesterol (mM)","Hemoglobin (g/L)","eGFR (ml/min/1.73m²)","Abnormal CTR","Abnormal EchoCG")

y <- dat[[1]]
res <- lapply(dat[,-1], function(x){
  fit <- lm(y~x)
  β <- round(coef(fit), 2)
  CI <- round(confint(fit), 2)
  P <- round(coef(summary(fit))[,4], 2)
  # Names the columns of CI
  colnames(CI) <- c("Lower", "Higher")
  # Bind columns together as dataset
  table2 <- as.data.frame(cbind(β, CI, P))
  # add brackes and line for later use in table
  table2$b <- "~"
  # order the columns
  table2 <- table2[,c("β","Lower","b","Higher", "P")]
  # Merge all columns in one
  library(tidyr)
  table2 = unite(table2, "95%CI", c( Lower, b, Higher), sep = "", remove=T)
  table2 <- table2[-1,]
  return(table2)
})
(res <- do.call(rbind,res))

library(ReporteRs)
library(magrittr)
# The script
#DDDDDD 灰色
options("ReporteRs-fontsize" = 12)

docx( ) %>% 
  addFlexTable(res %>%
                 # 设置字体
                 FlexTable(header.cell.props = cellProperties( background.color = "#DDDDDD"),
                           header.text.props = textBold(color = "black"),
                           add.rownames = TRUE ) %>%
                 #设置边界
                 setFlexTableBorders(inner.vertical = borderNone(),
                                     inner.horizontal = borderNone(),
                                     outer.vertical = borderNone(),
                                     outer.horizontal = borderProperties( color = "black",style = "solid", width = 2 )) %>% 
                 #斑马线
                 setZebraStyle(odd = "#FFFFFF", even = "#FFFFFF")
  ) %>%
  writeDoc(file = "单因素回归分析.docx")

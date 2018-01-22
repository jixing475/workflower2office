library(dplyr)
regression_result <- function(lm.fit="fit") {
  fit <- get(lm.fit)
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
}
dat <- select(TMAO_1_4_5,one_of("TMAO","age","sex","HDL","CHOL","Hemoglobin","eGFR","Cardiothoracic_ratio","heart_ultrasound"))
gg_na(dat)
dat$heart_ultrasound <- factor(dat$heart_ultrasound,levels = c("正常","异常"))
dat$Cardiothoracic_ratio <- factor(dat$Cardiothoracic_ratio,levels = c("正常","异常"))
dat$sex <- factor(dat$sex,levels = c("男","女"))

#colnames(dat) <- c("TMAO","Age(yr)","Female","History of hypertension","Cholesterol(mM)","eGFR(ml/min per 1.73m²)","Abnormal CTR","Abnormal EchoCG")


fit.1 <- glm(TMAO ~Cardiothoracic_ratio+age+sex,data = dat) 
fit.2 <- glm(TMAO ~Cardiothoracic_ratio+age+sex+HDL+CHOL,data = dat)
fit.3 <- glm(TMAO ~Cardiothoracic_ratio+age+sex+Hemoglobin+eGFR,data = dat)


(res.1 <- regression_result("fit.1"))
(res.2 <- regression_result("fit.2"))
(res.3 <- regression_result("fit.3"))
clipr::write_clip(res.3)
library(ReporteRs)
library(magrittr)
options("ReporteRs-fontsize" = 12)
docx( ) %>% 
  addFlexTable(Scr %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#DDDDDD"),
                           header.text.props = textBold(color = "black"),
                           add.rownames = TRUE ) %>%
                 setFlexTableBorders(inner.vertical = borderNone(),
                                     inner.horizontal = borderNone(),
                                     outer.vertical = borderNone(),
                                     outer.horizontal = borderProperties( color = "black",style = "solid", width = 2 )) %>% 
                 setZebraStyle(odd = "#FFFFFF", even = "#FFFFFF")
  ) %>%
  writeDoc(file = "res.docx")

##**************************************************##
##                                                  ## 
##          echocardiography                        ##
##                                                  ##
##**************************************************##
fit.UCG <- glm(TMAO ~heart_ultrasound+age+sex+HDL+CHOL+Hemoglobin+eGFR,data = dat)

(res.UCG <- regression_result("fit.UCG"))

docx( ) %>% 
  addFlexTable(res.UCG %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#DDDDDD"),
                           header.text.props = textBold(color = "black"),
                           add.rownames = TRUE ) %>%
                 setFlexTableBorders(inner.vertical = borderNone(),
                                     inner.horizontal = borderNone(),
                                     outer.vertical = borderNone(),
                                     outer.horizontal = borderProperties( color = "black",style = "solid", width = 2 )) %>% 
                 setZebraStyle(odd = "#FFFFFF", even = "#FFFFFF")
  ) %>%
  writeDoc(file = "resUCG.docx")


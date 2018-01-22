library(dplyr)
library(mgcv)
#dat <- select(TMAO_1_4_5,-one_of("LVEF","ECG","blood_sugar","NYHA","C_reactive_protein","Aspirin","total_bilirubin", "direct_bilirubin","indirect_bilirubin","touxi"))
dat <- select(TMAO_1_4_5,one_of("age","sex","hypertension","Cardiothoracic_ratio","heart_ultrasound","eGFR","CHOL",
                                 "Hemoglobin","C_reactive_protein","TMAO"))

gg_na(dat)
#dat <- to_impute(dat)
#dat <- na.omit(dat)
gg_na(dat)
dim(dat)
colnames(dat)

dat$Cardiothoracic_ratio <- factor(dat$Cardiothoracic_ratio,levels = c("正常","异常"))
gamfit.CTR <- mgcv::gam(Cardiothoracic_ratio~s(TMAO,k=4),data = dat,family = binomial)
plot(gamfit.CTR)
summary(gamfit.CTR)

dat$heart_ultrasound <- factor(dat$heart_ultrasound,levels = c("正常","异常"))
gamfit.UCG <- mgcv::gam(heart_ultrasound~s(TMAO,k=4),data = dat,family = binomial)
plot(gamfit.UCG)
summary(gamfit.UCG)

gamfit.eGFR <- mgcv::gam(eGFR~s(TMAO,k=4),data = dat)
plot(gamfit.eGFR)
summary(gamfit.eGFR)

par(mfrow = c(1,3))
gamfit <- gam::gam(Cardiothoracic_ratio~s(TMAO,df=4)+heart_ultrasound+eGFR,data = dat,family = binomial)
summary(gamfit)
plot(gamfit,se=T)
#model for CTR 1 2 3
glm(Cardiothoracic_ratio ~TMAO,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+eGFR,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+age,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+hypertension,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+heart_ultrasound,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+CHOL,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio ~TMAO+Hemoglobin,data = dat, family = binomial) %>% summary()#**0.02
glm(Cardiothoracic_ratio ~TMAO+C_reactive_protein,data = dat, family = binomial) %>% summary()
glm(Cardiothoracic_ratio~.,data = dat[,-c(4)],family = binomial)  %>% summary()
glm(Cardiothoracic_ratio~.,data = dat[,-c(4,5)],family = binomial)  %>% summary()
glm(Cardiothoracic_ratio~.,data = dat[,-c(4,5)],family = binomial)  %>% step()

#model for UCG
glm(heart_ultrasound ~TMAO,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+eGFR,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+age,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+hypertension,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+Cardiothoracic_ratio,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+CHOL,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+Hemoglobin,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound ~TMAO+C_reactive_protein,data = dat, family = binomial) %>% summary()
glm(heart_ultrasound~.,data = dat[,-c(3)],family = binomial)  %>% summary()
glm(heart_ultrasound~.,data = dat[,-c(3,5)],family = binomial)  %>% summary()
glm(heart_ultrasound~.,data = dat[,-c(3,5)],family = binomial)  %>% step()
glm(formula = heart_ultrasound ~ age + hypertension + CHOL + 
      Hemoglobin + C_reactive_protein + TMAO, family = binomial, 
    data = dat[, -c(3, 5)]) %>% summary()



##**************************************************##
##                                                  ## 
##          Model for TMAO                          ##
##                                                  ##
##**************************************************##
dat$heart_ultrasound <- factor(dat$heart_ultrasound,levels = c("正常","异常"))
dat$Cardiothoracic_ratio <- factor(dat$Cardiothoracic_ratio,levels = c("正常","异常"))
dat$hypertension <- factor(dat$hypertension,levels = c("无","有"))

glm(TMAO ~heart_ultrasound+age+CHOL,data = dat) %>% summary()

glm(TMAO ~heart_ultrasound+age+CHOL+hypertension+eGFR,data = dat) %>% summary()
##**************************************************##
##                                                  ## 
##          Model for TMAO                          ##
##                                                  ##
##**************************************************##

glm(TMAO ~heart_ultrasound+eGFR+age+CHOL+
      hypertension+C_reactive_protein,data = dat) %>% summary()
glm(TMAO ~eGFR,data = dat) %>% summary()#age 0.02
glm(TMAO ~eGFR+Hemoglobin,data = dat) %>% summary()
glm(TMAO ~eGFR+HDL,data = dat) %>% summary()
glm(TMAO ~eGFR+C_reactive_protein,data = dat) %>% summary()
glm(TMAO ~eGFR+hypertension,data = dat) %>% summary()

glm(TMAO ~Cardiothoracic_ratio+age+
      CHOL+eGFR+hypertension,data = dat) %>% summary()#no eGFR _Hemoglobin
glm(TMAO ~eGFR+Cardiothoracic_ratio+age+hypertension+
      HDL+Hemoglobin+C_reactive_protein,data = dat) %>% summary()#age

glm(TMAO ~eGFR+Cardiothoracic_ratio+age+hypertension+
      CHOL+HDL+Hemoglobin+C_reactive_protein,data = dat) %>% summary()##age 0.02






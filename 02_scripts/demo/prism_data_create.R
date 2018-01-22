library(clipr)
#TMAO~Cardiothoracic_ration
TMAO_1_4_5 %>%
  #filter(TMAO <= 100) %>%
  select(TMAO,Cardiothoracic_ratio) %>%
  arrange(Cardiothoracic_ratio) %>%
  #to_impute() %>%
  clipr::write_clip()

TMAO_1_4_5 %>%
  #filter(TMAO <= 100) %>%
  select(TMAO,Cardiothoracic_ratio) %>%
  filter(Cardiothoracic_ratio=="异常") %>% 
  .$TMAO %>% 
  quantile()
#======================== 
TMAO_1_4_5 %>%
  filter(LVEF_stat == "Normal") %>%
  select(TMAO,heart_ultrasound) %>%
  arrange(heart_ultrasound) %>%
  #to_impute() %>%
  clipr::write_clip()

TMAO_1_4_5 %>%
  #filter(TMAO <= 100) %>%
  select(TMAO,heart_ultrasound) %>%
  filter(heart_ultrasound=="正常") %>% 
  .$TMAO %>% 
  quantile()


library(radiant)
summary(correlation(dataset=eGFR_TMAO,vars=c("TMAO","eGFR","LVEF","age"),method="spearman"))









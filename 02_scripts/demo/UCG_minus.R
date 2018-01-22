library(readxl)
library(dplyr)
EchoCG <- read_excel("EchoCG.xlsx", sheet = "Sheet2")
EchoCG %>% select(one_of("心脏超声_minus","TMAO")) -> UCG

UCG <- UCG[UCG$心脏超声_minus != "NA",]

UCG %>% mutate(心脏超声状态 = if_else(心脏超声_minus == "正常", "正常", "异常")) %>% 
  select(one_of("心脏超声状态","TMAO")) %>% 
  arrange(心脏超声状态) %>% 
  clipr::write_clip()

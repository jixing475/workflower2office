healthy <- read_excel("./healthy.xlsx")
healthy %>%
  rename(TMAO = 血浆TMAO浓度) %>%
  select(TMAO) %>%
  mutate(CKD_stage="Control") -> healthy
healthy <- healthy[1:72,]
CKD <- TMAO_1_4_5 %>%
  select(TMAO,eGFR) %>%
  mutate(CKD_stage = cut(eGFR, c(0,15,60,100000), right=FALSE,labels = (3:1)) ) %>%
  filter(CKD_stage != "NA") %>%
  select(TMAO,CKD_stage)
  
CKD$CKD_stage <- case_when(
  CKD$CKD_stage == 1~ "stage_1_2",
  CKD$CKD_stage == 2~ "stage_3_4",
  CKD$CKD_stage == 3~ "stage_5"
)
## 合并数据
healthy_CKD <- bind_rows(healthy,CKD)

result <- explore(dataset = "healthy_CKD", vars = "TMAO", byvar = "CKD_stage", 
                  fun = c("length", "mean_rm", "median_rm"), nr = 4)
summary(result)

fit <- aov(TMAO~CKD_stage,data = healthy_CKD)
summary(fit)
##百分比
table(CKD$CKD_stage) %>%
  prop.table()%>%
  as.matrix() %>%
  apply(FUN=signif,digit=3,MARGIN = 2) %>%
  t() %>%
  t()*100 

## TMAO：CKD vs 健康 t检验
t.test(CKD$TMAO,healthy$TMAO)
#   ±
sd(CKD$TMAO)
sd(healthy$TMAO)


#2.6±3.1 vs 9.4±11.1 vs 47.2±43.6
CKD %>%
  filter(CKD_stage=="stage_5") %>%
  select(TMAO) %>%
  .$TMAO %>%
    sd()

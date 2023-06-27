# SIR
## load library
packages <- c("tidyverse", "plotly", "shiny", "leaflet", "DT", "flexdashboard", "data.table", "ggthemes", "ggrepel", "gridExtra")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
exactPoiCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  g<-data.frame(lower, upper)
  return(g)
}
## data import and explorer
### import data
pop = readRDS("data/size21.rds")
dz  = readRDS("data/dz.rds")
### exploer

pop
dz

##Choose a standard population:
### > Select a standard population to use as a reference for calculating the ASMR. The World Health Organization (WHO) standard population is commonly used.
sum(pop$weight)
pop %>%
  group_by(gender, age_gp) %>%
  summarise(pop = sum(weight)) -> ga_popu
source("rscript/source/ga_pyramid.R")
# ga_pyramid()

pop %>%
  group_by(gender, new_code, age_gp) %>%
  summarise(pop = sum(weight)) -> iga_popu
source("rscript/source/iga_pyramid.R")
iga_pyramid()

pop %>%
  group_by(gender, new_code, size_gp , age_gp) %>%
  summarise(pop = sum(weight)) -> siga_popu
source("rscript/source/siga_pyramid.R")
# siga_pyramid()

##Calculate the proportion of deaths by age group: 
dzgp <- dz %>%
  mutate(dz = case_when(
    dz_type2 == "해당없음" ~ "사고성재해", 
    TRUE ~ dz_type2
  )) %>%
  rename(new_code1 = new_code) %>%
  mutate(new_code = case_when(
    is.na(new_code1 ) ~ new_code2, 
    is.na(new_code2)  ~ new_code1
  ))

# saveRDS(dzgp, file = "data/dzgp.rds")

### 표준 집단의 관심 질병에 대한 연령병 발생률 계산 (age specific rate)



########################################  
####1 업종별
###################################
## 0) 관심 질병 선정
intdz = "사고성재해"
dzgp %>% count(dz) %>% arrange(desc(n))
## 1) 전체 집단에서 관심 질병의 발생수 계산 --> 전체 집단에서 관심 질병 율 계산
source("rscript/source/ana1.R")
tt = ana1(intdz)
# tt[[1]] 



########################################  
####1 사업장 크기 별 업종별
###################################
## 0) 관심 질병 선정
intdz = "사고성재해"
dzgp %>% count(dz) %>% arrange(desc(n))
## 1) 전체 집단에서 관심 질병의 발생수 계산 --> 전체 집단에서 관심 질병 율 계산
source("rscript/source/ana2.R")
tt2 = ana2(intdz)
# tt2[[1]] 


############### 분 석 ##############

####### 0.사고성재해 #######

tt1 = ana1 ("사고성재해")
tt1[[1]] 
tt1[[2]] %>% print(n=100) -> Table_iga_0
# saveRDS(Table_iga_0, file = "data/Table_iga_0.rds")
tt1[[3]] # Fig_iga_0

tt2 = ana2("사고성재해")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_0
# saveRDS(Table_siga_0, file = "data/Table_siga_0.rds")
# tt2[[3]]

####### 1.신체부담작업 #######

tt1 = ana1 ("신체부담작업")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_1
# saveRDS(Table_iga_1, file = "data/Table_iga_1.rds")
tt1[[3]] # Fig_iga_1

tt2 = ana2("신체부담작업")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_1
# saveRDS(Table_siga_1, file = "data/Table_siga_1.rds")
# tt2[[3]]

####### 2.사고성요통 #######

tt1 = ana1 ("사고성요통")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_2
# saveRDS(Table_iga_2, file = "data/Table_iga_2.rds")
tt1[[3]] # Fig_iga_2

tt2 = ana2("사고성요통")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_2
# saveRDS(Table_siga_2, file = "data/Table_siga_2.rds")
# tt2[[3]]

####### 3.소음성난청 #######

tt1 = ana1 ("소음성난청")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_3
# saveRDS(Table_iga_3, file = "data/Table_iga_3.rds")
tt1[[3]] # Fig_iga_3

tt2 = ana2("소음성난청")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_3
# saveRDS(Table_siga_3, file = "data/Table_siga_3.rds")
# tt2[[3]]

####### 4.비사고성요통 #######

tt1 = ana1 ("비사고성요통")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_4
# saveRDS(Table_iga_4, file = "data/Table_iga_4.rds")
tt1[[3]] # Fig_iga_4

tt2 = ana2("비사고성요통")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_4
# saveRDS(Table_siga_4, file = "data/Table_siga_4.rds")
# tt2[[3]] 

####### 5.진폐증 #######

tt1 = ana1 ("진폐증")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_5
# saveRDS(Table_iga_5, file = "data/Table_iga_5.rds")
tt1[[3]] # Fig_iga_5

tt2 = ana2("진폐증")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_5
# saveRDS(Table_siga_5, file = "data/Table_siga_5.rds")
# tt2[[3]] 

####### 6.뇌혈관질환 #######

tt1 = ana1 ("뇌혈관질환")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_6
# saveRDS(Table_iga_6, file = "data/Table_iga_6.rds")
tt1[[3]] # Fig_iga_6

tt2 = ana2("뇌혈관질환")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_6
# saveRDS(Table_siga_6, file = "data/Table_siga_6.rds")
# tt2[[3]]

####### 7.심장질환 #######

tt1 = ana1 ("심장질환")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_7
# saveRDS(Table_iga_7, file = "data/Table_iga_7.rds")
tt1[[3]] # Fig_iga_7

tt2 = ana2("심장질환")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_7
# saveRDS(Table_siga_7, file = "data/Table_siga_7.rds")
# tt2[[3]] 

####### 8.정신질환 #######

tt1 = ana1 ("정신질환")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_8
# saveRDS(Table_iga_8, file = "data/Table_iga_8.rds")
tt1[[3]] # Fig_iga_8

tt2 = ana2("정신질환")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_8
# saveRDS(Table_siga_8, file = "data/Table_siga_8.rds")
# tt2[[3]] 

####### 9.직업성암 #######

tt1 = ana1 ("직업성암")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_9
# saveRDS(Table_iga_9, file = "data/Table_iga_9.rds")
tt1[[3]] # Fig_iga_9

tt2 = ana2("직업성암")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_9
# saveRDS(Table_siga_9, file = "data/Table_siga_9.rds")
# tt2[[3]]

####### 10.감염성질환 #######

tt1 = ana1 ("감염성질환")
tt1[[1]] 
tt1[[2]] %>% print(n=100)  -> Table_iga_10
# saveRDS(Table_iga_10, file = "data/Table_iga_10.rds")
tt1[[3]] # Fig_iga_10

tt2 = ana2("감염성질환")
tt2[[1]]
tt2[[2]] %>% print(n=500) -> Table_siga_10
# saveRDS(Table_siga_10, file = "data/Table_siga_10.rds")
# tt2[[3]]


########## iga & siga print ##########

library(htmlTable)

Table_iga_0 %>% htmlTable(caption = "사고성재해_iga")
Table_iga_1 %>% htmlTable(caption = "신체부담작업_iga")
Table_iga_2 %>% htmlTable(caption = "사고성요통_iga")
Table_iga_3 %>% htmlTable(caption = "소음성난청_iga")
Table_iga_4 %>% htmlTable(caption = "비사고성요통_iga")
Table_iga_5 %>% htmlTable(caption = "진폐증_iga")
Table_iga_6 %>% htmlTable(caption = "뇌혈관질환_iga")
Table_iga_7 %>% htmlTable(caption = "심장질환_iga")
Table_iga_8 %>% htmlTable(caption = "정신질환_iga")
Table_iga_9 %>% htmlTable(caption = "직업성암_iga")
Table_iga_10 %>% htmlTable(caption = "감염성질환_iga")


Table_siga_0 %>% htmlTable(caption = "사고성재해_siga")
Table_siga_1 %>% htmlTable(caption = "신체부담작업_siga")
Table_siga_2 %>% htmlTable(caption = "사고성요통_siga")
Table_siga_3 %>% htmlTable(caption = "소음성난청_siga")
Table_siga_4 %>% htmlTable(caption = "비사고성요통_siga")
Table_siga_5 %>% htmlTable(caption = "진폐증_siga")
Table_siga_6 %>% htmlTable(caption = "뇌혈관질환_siga")
Table_siga_7 %>% htmlTable(caption = "심장질환_siga")
Table_siga_8 %>% htmlTable(caption = "정신질환_siga")
Table_siga_9 %>% htmlTable(caption = "직업성암_siga")
Table_siga_10 %>% htmlTable(caption = "감염성질환_siga")

########## iga summary print ##########

igaPrint1 <- Table_iga_10 %>% filter(gender == 1) %>%  
  select(new_code, Tobs, SIR, LL, UL, SIR95CI) 
  
igaPrint2 <- Table_iga_10 %>% filter(gender == 2) %>%  
  select(new_code, Tobs, SIR, LL, UL, SIR95CI)

outerjoin <- merge(x = igaPrint1, y = igaPrint2, 
                   by = c('new_code'), all = TRUE) 

outerjoin <- outerjoin %>% 
  select(new_code, gender.x, Tobs.x, SIR95CI.x, gender.y, Tobs.y, SIR95CI.y) 

outerjoin <- rename(outerjoin, "new_code"="new_code", "Men"="gender.x", "Observed in Men"="Tobs.x", "SIR(95% CI) of Men"="SIR95CI.x", "Women"="gender.y", "Observed in Women"="Tobs.y","SIR(95% CI) of Women"="SIR95CI.y")

outerjoin <- outerjoin %>% select("new_code", "Observed in Men", "SIR(95% CI) of Men", "Observed in Women", "SIR(95% CI) of Women")

outerjoin %>% setNames(c("new_code", "Observed", "SIR(95% CI)", "Observed", "SIR(95% CI)")) %>% 
  htmlTable(cgroup = c("","Men", "Women"), n.cgroup = c(1,2,2),
            rnames = FALSE, 
            caption = "SIR(95% CI) of 감염성질환 by Industry")

########## siga summary print ##########

sigaPrint1 <- Table_siga_10 %>% filter(gender == 1) %>%  
  select(new_code, size_gp, SIR95CI) %>% 
  mutate(size = case_when(
    size_gp == "1" ~ "<5",
    size_gp == "2" ~ "<10",
    size_gp == "3" ~ "<30",
    size_gp == "4" ~ "<100",
    size_gp == "5" ~ "<300",
    TRUE ~ "≥300"))

sigaPrint2 <- Table_siga_10 %>% filter(gender == 2) %>%  
  select(new_code, size_gp, SIR95CI) %>% 
  mutate(size = case_when(
    size_gp == "1" ~ "<5",
    size_gp == "2" ~ "<10",
    size_gp == "3" ~ "<30",
    size_gp == "4" ~ "<100",
    size_gp == "5" ~ "<300",
    TRUE ~ "≥300"))

outerjoin <- merge(
  x = sigaPrint1, 
  y = sigaPrint2, 
  by = c('new_code', 'size_gp'), 
  all = TRUE) 

outerjoin %>% 
  select(new_code, size.x, SIR95CI.x, SIR95CI.y) 

outerjoin <- rename(outerjoin, "new_code"="new_code", "size"="size.x", "SIR(95% CI) of Men"="SIR95CI.x", "SIR(95% CI) of Women"="SIR95CI.y")
  
outerjoin <- outerjoin %>% select("new_code", "size", "SIR(95% CI) of Men", "SIR(95% CI) of Women")

outerjoin %>% setNames(c("new_code", "size", "Men", "Women")) %>% 
  htmlTable(
    cgroup = c("","", "SIR(95% CI)"),
    n.cgroup = c(1,1,2),
    rnames = FALSE, 
    caption = "SIR(95% CI) of 감염성질환")


a1 %>% htmlTable() 



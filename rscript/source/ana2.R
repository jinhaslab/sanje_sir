ana2 = function(x){

intdz = x
dzgp %>% filter(dz ==intdz) %>%
  group_by(gender, age_gp) %>%
  count()  -> ga_dz
#source("rscript/source/ga_dz_pyramid.R")

#ga_dz_pyramid()
dzgp %>% filter(dz ==intdz) %>%
  group_by(gender, new_code, age_gp) %>%
  count()  -> iga_dz
#source("rscript/source/iga_dz_pyramid.R")
#iga_dz_pyramid()
dzgp %>% filter(dz ==intdz) %>%
  group_by(gender, new_code, size_gp , age_gp) %>%
  count()  -> siga_dz
source("rscript/source/siga_dz_pyramid.R")
#siga_dz_pyramid()

############################################
## 2. Apply the age-specific disease rates:
##############################################
# 0 basic matrix
## 성, 연령, 업종
## 성, 연령, 업종, 규모

# dz / population
# 1) 전체 집단에서 관심 질병 율 계산 --> 표준 집단의 성연령별 발생률
#ga_popu: population
#ga_dz: dz
# ga_dz/ga_popu --> stand rate
ga_popu %>%
  left_join(ga_dz, by = c("gender", "age_gp")) %>%
  mutate(across(c("n"), ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(gasRate = n/pop) %>%
  select(gender, age_gp, gasRate)-> gasRate

# expected and observed
## expected : population(i) * gasRate (standard)
# inter = industry



siga_popu %>%
  left_join(siga_dz, by = c("gender", "new_code","size_gp", "age_gp")) %>%
  left_join(gasRate, by = c("gender", "age_gp")) %>%
  mutate(across(c("n"), ~ifelse(is.na(.x), 0, .x))) %>%
  mutate(expected = pop * gasRate) %>%
  group_by(gender, new_code, size_gp) %>%
  mutate(Tobs = sum(n), Lobs = exactPoiCI(Tobs)$lower, Uobs = exactPoiCI(Tobs)$upper,
         Texp = sum(expected), 
         SIR  = Tobs/Texp, 
         LL   =  Lobs/Texp,
         UL   =  Uobs/Texp )  %>%
  mutate(SIR95CI = sprintf("%0.2f (%0.2f-%0.2f)", SIR, LL, UL)) -> siga_ratio

siga_ratio %>%
  select(gender, new_code, size_gp, Tobs, SIR, LL, UL, SIR95CI) %>%
  unique() -> siga_ratio_smry

# Create the plot
fig2 = siga_ratio_smry %>%
  ggplot( aes(x = Tobs, y = SIR, ymin = LL, ymax = UL, color= size_gp )) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  facet_wrap(~ gender, ncol = 2) + # facet by gender
  scale_x_log10() +
  scale_y_log10() +
  xlab("Tobs") +
  ylab("SIR") +
  labs(title = sprintf("SIR and Observation of %s", intdz), x = "Tobs", y = "SIR") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_label_repel(aes(label = sprintf("%s-%s",new_code, size_gp)), 
                   fill = NA, # 투명하게 해서 겹쳐도 보이게
                   alpha =1, size = 3, # 작게
                   box.padding = 0.4,  # 분별해서
                   segment.size =0.1,  # 선 
                   force = 2)  +
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", size = 15, face = "bold"),
    axis.title = element_text(family = "Times New Roman", size = 13, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "bottom",
    legend.justification = "center"
  )
out = list(siga_ratio, siga_ratio_smry, fig2)

}
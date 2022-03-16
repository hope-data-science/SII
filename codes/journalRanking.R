
library(pacman)
p_load(tidyfst,tidyverse)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_jif = eco_ut %>% 
  summarise_dt(jif = sum(TC_2019)/.N,by = ISSN)

eco_jif %>%
  mutate_dt(jif_rank = min_rank(-jif)) -> jif_rank

eco_ut %>% 
  mutate_dt(rank = min_rank(-TC_2019)) %>% 
  summarise_dt(
    prp = mean(rank)/nrow(eco_ut) * 100,by = .(ISSN,Source)
  ) %>% 
  mutate_dt(prp_rank = min_rank(prp)) -> prp_rank


jif_rank %>% 
  inner_join_dt(prp_rank,by = "ISSN") -> dt

dt %>% 
  transmute(
    Journal = Source,
    ISSN,jif,jif_rank,
    prp,prp_rank
  ) %>% 
  write_excel_csv("output/prp_jif_rank.csv")

dt %>% mutate(rank_delta = jif_rank - prp_rank) %>% 
  arrange(rank_delta) %>% 
  # tail(10)
  head(10)

dt %>% mutate(rank_delta = jif_rank - prp_rank) %>% 
  filter(rank_delta == max(rank_delta)) -> hl

dt %>% 
  ggplot(aes(jif_rank,prp_rank)) +
  geom_point() +
  # geom_point(data = hl,color = "red")+
  # geom_text(data = hl,aes(label = Source),color = "red",nudge_y = -5,nudge_x = 20)+
  geom_abline(slope = 1,linetype = "dashed") +
  ggpubr::stat_cor() +
  labs(x = "\nJIF rank",y = "PRP rank\n") +
  theme_bw()

ggsave("output/prp_jif_journal_rank.png",width = 5,height = 5)



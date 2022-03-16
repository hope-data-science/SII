
library(pacman)
p_load(tidyfst,ggplot2)

fread("data_prepare/eco_jif.csv")
fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% 
  transmute_dt(top = 1:.N,journal_no = cumsum(!duplicated(ISSN))) -> df

df %>% filter_dt(top <= .N*0.01)
df %>% filter_dt(top <= .N*0.05)
df %>% filter_dt(top <= .N*0.1)

df%>% 
  ggplot(aes(top,journal_no)) +
  geom_line() + labs(x = "Top paper no.",y = "Journal no.") +
  theme_bw()
  

df%>% 
  mutate_dt(top = top/max(top),journal_no = journal_no/max(journal_no)) %>% 
  mutate(top0 = round(top*100,0)) %>% 
  # filter(top0 != 0) %>% 
  distinct(top0,.keep_all = T) %>% 
  ggplot(aes(top,journal_no)) +
  geom_line() + 
  labs(
    x = expression('Cumulative percentage of top paper number ('~italic(p)~')'),
    y = "Cumulative percentage of journal number with non-zero SII"
  ) +
  geom_abline(slope = 1,linetype = "dashed") +
  expand_limits(x = 1.01) +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  theme_bw()+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

ggsave("output/minor_topPaper_jrNo.png",width = 5,height = 5)

############
df%>% 
  mutate_dt(top = top/max(top),journal_no = journal_no/max(journal_no)) %>% 
  filter_dt(journal_no>=1)
df%>% 
  mutate_dt(top = top/max(top),journal_no = journal_no/max(journal_no)) %>% 
  filter_dt(journal_no>=.5)

############
library(pacman)
p_load(tidyfst,dplyr,ggplot2)


fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut
eco_ut %>% count(ISSN,name = "size") -> issn_size
eco_ut %>% distinct_dt(ISSN) -> issn

p2sii = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(n,to = 0) %>% 
    mutate(n = n/sum(n)) %>% 
    mutate(p = p) %>% 
    rename(sii = n) %>% 
    mutate(sii_rank = min_rank(-sii))
}

lapply(1:100,p2sii) %>% 
  rbindlist() -> all_sii

all_sii %>% 
  inner_join(issn_size,by = "ISSN") %>% 
  summarise_dt(cor = cor(size,sii,method = "spearman"),by = p) %>% 
  ggplot(aes(p/100,cor)) + geom_point() + geom_line() +
  hrbrthemes::scale_x_percent() +
  expand_limits(x = 1.01) +
  labs(
    x = expression('Cumulative percentage of top paper number ('~italic(p)~')'),
    y = "Correlation between SII and journal productivity"
  ) + theme_bw() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
ggsave("output/minor_sii_productivity.png",width = 5,height = 5)  



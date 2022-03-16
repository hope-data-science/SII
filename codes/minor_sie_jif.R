library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr,ggpubr)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% count(ISSN,name = "no") -> issn

eco_jif = eco_ut %>% 
  summarise_dt(jif = sum(TC_2019)/.N,by = ISSN)

p2siie = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    mutate(sii = n/sum(n)) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(to = 0) %>% 
    mutate(p = p) %>% 
    mutate(sie = n/no)
}

lapply(1:100,p2siie) %>% 
  rbindlist() -> siie

siie %>% 
  inner_join_dt(eco_jif,by = "ISSN") %>% 
  .[,.(r = cor(sie,jif,method = "spearman")),by = p] %>% 
  ggplot(aes(p/100,r)) + 
  geom_point(size = .5) + geom_line() +
  hrbrthemes::scale_x_percent() +
  expand_limits(x = 1.01) +
  theme_bw() + labs(
    x = expression('Cumulative percentage of top paper number ('~italic(p)~')'),
    y = "Correlation between JIF and SIE"
  )+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
ggsave("output/minor_topPaper_cor_jif_sie.png",width = 5,height = 5)

siie %>% 
  inner_join_dt(eco_jif,by = "ISSN") %>% 
  .[,.(r = cor(sie,jif,method = "spearman")),by = p] %>% 
  .[r==max(r,na.rm = T)]

siie %>% 
  inner_join_dt(eco_jif,by = "ISSN") %>% 
  filter_dt(p %in% 2^(3:6)) %>% 
  mutate(p_label = str_c("p = ",p)) %>% 
  mutate(p_label = reorder(p_label,p)) %>% 
  ggplot(aes(jif,sie)) +
  geom_point() +
  geom_smooth(method = "lm",se = F,linetype = "dashed") +
  facet_wrap(~p_label,nrow = 2) +
  labs(y = "SIE",x = "JIF") +
  ggpubr::stat_cor(label.y = 1.1,method = "spearman") +
  theme_bw()



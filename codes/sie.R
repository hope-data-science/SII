
library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr)

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

# lapply(c(1,5,10,50),p2siie) %>% 
#   rbindlist() -> siie

lapply(1:50,p2siie) %>% 
  rbindlist() -> siie

siie %>% 
  mutate(p_label = str_c("p = ",p)) %>% 
  mutate(p_label = reorder(p_label,p)) %>% 
  ggplot(aes(sie,sii)) +
  geom_point() +
  facet_wrap(~p_label,scales = "free_x",ncol = 5) +
  labs(x = "SIE",y = "SII") +
  ggpubr::stat_cor() +
  theme_bw()
ggsave("output/sii_sie.png",width = 30,height = 40,units = "cm")

# lapply(c(.01,.1,1,2),p2siie) %>% 
#   rbindlist() -> siie


######################################################################

lapply(1:100,p2siie) %>% 
  rbindlist() -> siie

# sys_time_print({
#   lapply(100/(33865:1),p2siie) %>% 
#     rbindlist() -> siie
# })

eco_ut %>% 
  distinct(ISSN,Source) %>% 
  filter(Source %in% c("ECOSPHERE","ECOLOGY")) -> sel_issn

siie %>% 
  inner_join(sel_issn,by = "ISSN") %>% 
  ggplot(aes(p/100,sie,color = Source)) +
  geom_point() +
  geom_line()  +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  geom_abline(slope = 1,linetype = "dashed")+
  expand_limits(x = 1.01) +
  labs(
    x = expression('\nCumulative percentage of top paper number ('~italic(p)~')'),
    # x = "\nCumulative percentage of top paper number",
    y = "SIE\n",
    color = "Journal"
  ) +
  theme_bw() +
  theme(legend.position = c(.2, .85)) +
  theme(legend.background = element_rect(fill = "white", colour = "black"))
ggsave("output/sie_p.png",width = 5,height = 5)  

siie %>% 
  inner_join(sel_issn,by = "ISSN") %>% filter(p==10)




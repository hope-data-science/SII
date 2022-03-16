
library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr,stringr,ggpubr)

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



lapply(c(.1,.5,1,5,10,20),p2siie) %>% 
  rbindlist() -> siie

siie %>% 
  # filter(sie != 0) %>%
  mutate(p_label = str_c("p = ",p)) %>% 
  mutate(p_label = reorder(p_label,p)) %>% 
  ggplot(aes(no,sie)) +
  geom_point() +
  geom_smooth(method = "lm",se = F,linetype = "dashed") +
  facet_wrap(~p_label,scales = "free",nrow = 2) +
  labs(y = "SIE",x = "Journal productivity") +
  ggpubr::stat_cor(label.x.npc = .4,method = "spearman") +
  theme_bw()+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

ggsave("output/minor_JourProd_sie.png",width = 15*1.6,height = 15,units = "cm")

siie %>% 
  filter(p == .1) %>%
  slice_max(sie,n = 15) %>% 
  mutate(p_label = str_c("p = ",p)) %>% 
  mutate(p_label = reorder(p_label,p)) %>% 
  ggplot(aes(no,sie)) +
  geom_point() +
  geom_smooth(method = "lm",se = F,linetype = "dashed") +
  # facet_wrap(~p_label,scales = "free",nrow = 1) +
  labs(y = "SIE",x = "Journal productivity") +
  ggpubr::stat_cor(label.x.npc = .4,method = "spearman") +
  theme_bw()+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
ggsave("output/minor_JourProd_sie_top15sie.png",width = 9,height = 9,units = "cm")

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
    x = expression('Cumulative percentage of top paper number ('~italic(p)~')'),
    y = "SIE",
    color = "Journal"
  ) +
  theme_bw() +
  theme(legend.position = c(.2, .85)) +
  theme(legend.background = element_rect(fill = "white", colour = "black"))+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )
ggsave("output/minor_sie_p.png",width = 5,height = 5)  

siie %>% 
  inner_join(sel_issn,by = "ISSN") %>% filter(p==10)




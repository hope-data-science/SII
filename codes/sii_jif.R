
library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% distinct_dt(ISSN) -> issn

eco_jif = eco_ut %>% 
  summarise_dt(jif = sum(TC_2019)/.N,by = ISSN)

p2sii = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    mutate(n = n/sum(n)) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(n,to = 0) %>% 
    mutate(p = p) %>% 
    rename(sii = n)
}

lapply(1:100,p2sii) %>% 
  rbindlist() -> all_sii

get_cor = function(i){
  all_sii[p==i] %>% 
    merge(eco_jif,by = "ISSN") -> dt
  
  dt[,cor(sii,jif,method = "spearman")] -> r
  data.table(p = i,r = r)
}

lapply(1:100,get_cor) %>% 
  rbindlist() -> all_cor

all_cor

all_cor[r==max(r)]

all_cor %>% 
  ggplot(aes(p,r)) + 
  geom_point() + geom_line() +
  gghighlight::gghighlight(r == max(r),label_key = p)

all_cor %>% 
  ggplot(aes(p/100,r)) + 
  geom_point(size = .5) + geom_line() +
  hrbrthemes::scale_x_percent() +
  expand_limits(x = 1.01) +
  theme_bw() + labs(
    x = expression('\nCumulative percentage of top paper number ('~italic(p)~')'),
    # x = "\nCumulative percentage of top paper number",
    y = "Spearman correlation between JIF and SII\n"
  )
ggsave("output/topPaper_cor_jif_sii.png",width = 5,height = 5)

#######################################################################
## discard

library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% distinct_dt(ISSN) -> issn

eco_jif = eco_ut %>% 
  summarise_dt(jif = sum(TC_2019)/.N,by = ISSN)

p2rank = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    mutate(n = n/sum(n)) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(n,to = 0) %>% 
    mutate(rank = min_rank(-n),p = p) %>% 
    select(-n)
}
jif_rank = eco_jif %>% 
  mutate(jif_rank = min_rank(-jif))

lapply(1:100,p2rank) %>% 
  rbindlist() -> all_ranks

get_cor = function(i){
  p2rank(i) %>% 
    merge(eco_jif_csi,by = "ISSN") -> dt
  
  dt[,cor(rank,jif_rank,method = "spearman")] -> r
  data.table(p = i,r = r)
}

lapply(1:100,get_cor) %>% 
  rbindlist() -> all_cor

all_cor

all_cor[r==max(r)]

all_cor %>% 
  ggplot(aes(p,r)) + 
  geom_point() + geom_line() +
  gghighlight::gghighlight(r == max(r),label_key = p)



all_cor %>% 
  ggplot(aes(p/100,r)) + 
  geom_point(size = .5) + geom_line() +
  hrbrthemes::scale_x_percent() +
  expand_limits(x = 1.01) +
  theme_bw() + labs(
    # x = expression('\nCumulative percentage of top paper number ('~italic(p)~')'),
    x = "\nCumulative percentage of top paper number",
    y = "Spearman correlation between JIF rank and SII rank\n"
  )

ggsave("output/topPaper_cor_jif_sii.png",width = 5,height = 5)

#######################################################################


library(pacman)
p_load(tidyfst,ggplot2,data.table,dplyr)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% distinct_dt(ISSN) -> issn

eco_jif = eco_ut %>% 
  summarise_dt(jif = sum(TC_2019)/.N,by = ISSN)

a2b = function(a,b){
  CJ(a,b)[,(sum(a>b) + sum(a==b)/2)/.N]
}

csi_issn = function(issn){
  a = eco_ut[ISSN == issn,TC_2019]
  b = eco_ut[ISSN != issn,TC_2019]
  data.table(ISSN = issn,csi = a2b(a,b))
}

sys_time_print({
  eco_ut[,ISSN] %>% unique() %>% 
    lapply(csi_issn) %>% 
    rbindlist() -> eco_csi
}) # [1] "Finished in 20.7s elapsed (41.7s cpu)"

# sys_time_print({
#   csi_issn("2041-210X") -> test
# })

# eco_h = eco_ut %>% 
#   arrange_dt(ISSN,-TC_2019) %>% 
#   mutate_dt(id = 1:.N,by = ISSN) %>% 
#   .[,.SD[id<=TC_2019][.N],by = ISSN] %>% 
#   select_dt(ISSN,h_index = id) %>% 
#   arrange_dt(-h_index)

p2rank = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(n,to = 0) %>% 
    mutate(rank = min_rank(-n),p = p) %>% 
    select(-n)
}

# eco_jif %>% mutate_dt(jif_rank = min_rank(-jif)) %>% 
#   select_dt(-jif) -> jif_rank
# eco_csi %>% mutate_dt(jif_rank = min_rank(-csi)) %>% 
#   select_dt(-csi) %>% rename_dt(ISSN = issn)-> jif_rank

eco_jif %>% merge(eco_csi,by = "ISSN") %>% 
  mutate(jif_rank = min_rank(-jif),csi_rank = min_rank(-csi))-> eco_jif_csi

lapply(1:100,p2rank) %>% 
  rbindlist() -> all_ranks

get_cor = function(i){
  p2rank(i) %>% 
    merge(eco_jif_csi,by = "ISSN") -> dt
  
  # dt[,(rank-jif_rank)^2 %>% mean %>% sqrt] -> rmse
  dt[,cor(rank,jif_rank)] -> r_jif
  dt[,cor(rank,csi_rank)] -> r_csi
  # data.table(p = i,r = r,rmse = rmse)
  data.table(p = i,r_jif,r_csi)
}

lapply(1:100,get_cor) %>% 
  rbindlist() -> all_cor

all_cor %>% 
  longer_dt(p) %>% 
  ggplot(aes(p,value,color = name)) +
  geom_line() +
  gghighlight::gghighlight(value == max(value),label_key = p,use_group_by = T)


# lapply(seq(1,100,.1),get_cor) %>% 
#   rbindlist() -> all_cor

# all_cor %>% ggplot(aes(p,r)) +
#   geom_line() + geom_point() +
#   gghighlight::gghighlight(r == max(r),label_key = p)
# 
# # all_cor %>% ggplot(aes(p,rmse)) +
# #   geom_line()+ geom_point() +
# #   gghighlight::gghighlight(rmse == min(rmse),label_key = p)
# all_cor




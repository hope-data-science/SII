
library(pacman)
p_load(tidyfst,dplyr,ggplot2)

data.table(
  journal = c("Ecology",
              "Ecosphere",
              "Frontiers in Ecology and the Environment",
              "Ecological Applications",
              "Ecological Monographs"),
  EISSN = c("1939-9170","2150-8925","1540-9309","1939-5582","1557-7015")
) %>%  merge(fread("data_prepare/issn_journal.csv")) %>% 
  select(ISSN,journal) -> sel_journals

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% count(ISSN,name = "size") -> issn_size

eco_ut %>% distinct_dt(ISSN) -> issn


# fwrite(eco_ut %>% unique(),"data_prepare/eco_ut_info.csv")

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
    rename(sii = n) %>% 
    mutate(sii_rank = min_rank(-sii)) %>% 
    inner_join(sel_journals,by = "ISSN")
}

lapply(1:100,p2sii) %>% 
  rbindlist() -> all_sii

all_sii[journal %in% c("Ecology","Ecosphere")] %>%
  wider_dt(p,name = "journal",value = "sii") %>%
  .[Ecology < Ecosphere]

all_sii[journal %in% c("Ecology","Ecosphere")] %>%
  wider_dt(p,name = "journal",value = "sii") 

all_sii[journal %in% c("Ecology","Ecosphere")] %>%
  wider_dt(p,name = "journal",value = "sii") %>% 
  .[,.SD[c(1,.N)],by = .(Ecology,Ecosphere)] %>% 
  unique()

all_sii%>%
  wider_dt(p,name = "journal",value = "sii") %>% 
  .[,.SD[c(1,.N)],by = setdiff(names(.),"p")] %>% 
  unique()

all_sii %>% 
  ggplot(aes(p,sii_rank,color = journal)) +
  geom_point() + geom_line()

all_sii %>% 
  ggplot(aes(p,sii,color = journal)) +
  geom_point() + geom_line() + scale_x_log10()


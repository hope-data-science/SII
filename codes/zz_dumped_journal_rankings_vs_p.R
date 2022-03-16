
library(pacman)
p_load(tidyfst,tidyverse)

fread("data_prepare/eco_ut_info.csv") %>% 
  arrange_dt(-TC_2019) -> eco_ut

eco_ut %>% distinct_dt(ISSN) -> issn


p2rank = function(p){
  eco_ut %>% 
    slice_max(TC_2019,prop = p/100) %>% 
    count(ISSN) %>% 
    full_join(issn,by = "ISSN") %>% 
    replace_na_dt(n,to = 0) %>% 
    mutate(rank = min_rank(-n),p = p) %>% 
    select(-n)
}

lapply(1:100,p2rank) %>% 
  rbindlist() -> all_ranks

get_diff = function(i){
  all_ranks[p==i][order(ISSN),rank] -> a
  all_ranks[p==(i-1)][order(ISSN),rank] -> b
  (a - b)^2 %>% mean %>% sqrt
}  

# sapply(2:100,get_diff) 

data.table(p = 1:100,delta_rank = c(NA,sapply(2:100,get_diff))) -> dt

dt %>% 
  ggplot(aes(p,delta_rank)) + 
  geom_line() + geom_point() +
  labs(x = "\np(%)",y = "Change of journal ranking (RMSE)\n",
       title = "Change of journal ranking when p increases (1 % at a time)")

#############################################################################
all_ranks %>% 
  ggplot(aes(p,rank)) +
  geom_line() + facet_wrap(~ISSN)



#############################################################################

# init = issn %>% mutate(rank = 1)
# all = data.table()
# 
# sys_time_print({
#   for(i in 1:nrow(eco_ut)){ # nrow(eco_ut)
#     eco_ut[1:i] %>% 
#       count_dt(ISSN) %>% 
#       full_join(issn,by = "ISSN") %>% 
#       .[is.na(n),n:=0] %>% 
#       mutate(rank = min_rank(-n)) -> new_rank
#     new_rank %>% 
#       full_join(init,by = "ISSN") %>% 
#       .[,(rank.x-rank.y)^2 %>% mean %>% sqrt] -> rank_diff 
#     data.table(n = i,delta_rank = rank_diff) %>% 
#       rbind(all,.) -> all
#     new_rank %>% select(-n) -> init
#   }
# })
# # [1] "Finished in 00:04:46 elapsed (00:04:38 cpu)"
# 
# all
# all %>% 
#   ggplot(aes(n,delta_rank)) + 
#   geom_line(alpha = .5) + geom_point() +
#   labs(x = "\nn(No. of top paper)",y = "Change of journal ranking (RMSE)\n",
#        title = "Change of journal ranking when n increases (1 paper at a time)")
# 
# fwrite(all,"data_prepare/n_journal_rmse.csv")








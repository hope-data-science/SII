
library(pacman)
p_load(tidyverse)

rep(1:4,4:1) %>% 
  enframe() %>% 
  mutate(journal)


tibble(
  journal = c(rep("C",3),rep("B",3),rep("A",3)),
  citation = rep(1:3,each = 3) 
)%>%
# tibble(
#   journal = c(rep("C",4),rep("B",3),rep("A",3)),
#   citation = rep(1:4,4:1) 
# )%>%
  arrange(-citation) %>% 
  transmute_dt(top = 1:.N,journal_no = cumsum(!duplicated(journal))) %>% 
  mutate_dt(top = top/max(top),journal_no = journal_no/max(journal_no)) %>% 
  mutate(top0 = round(top*100,0)) %>% 
  distinct(top0,.keep_all = T) %>% 
  ggplot(aes(top,journal_no)) +
  geom_line() + 
  labs(
    x = expression('\nCumulative percentage of top paper number ('~italic(p)~')'),
    y = "Cumulative percentage of journal number with non-zero SII\n"
  ) +
  geom_abline(slope = 1,linetype = "dashed") +
  expand_limits(x = 0:1,y = 0:1) +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  theme_bw()

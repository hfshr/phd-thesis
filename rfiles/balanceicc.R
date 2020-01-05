library(tidyverse)
library(readxl)
library(janitor)

dat <- read_excel("data/balancecom/balancecom.xlsx") %>% 
  clean_names()

d <- dat %>% 
  select(participant, grand_total, set)%>% 
  filter(set == 1) %>% 
  cbind(., dat %>% 
          select(participant, grand_total, set) %>% 
          filter(set == 2) %>%
          rename(grand_total_2 = "grand_total") %>% 
          select(grand_total_2)) %>% 
  select(-set, -participant) 

icc <- icc(d, model = "twoway", type = "agreement")
save(icc, file = "datavars/balanceicc.rds")


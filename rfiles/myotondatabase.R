library(tidyverse)
library(readxl)

myoton <- read_excel("data/rawdata.xlsx",
                sheet = "Myoton database", 
                col_names = TRUE, 
                na = c("-", "NA")) %>% 
  rename_all(funs(make.names(.))) %>%
  rename_all(funs(tolower(.))) %>% 
  select(measurement.time, 
         subject.name, 
         id, 
         dominant.side,
         gender,
         pattern, 
         object, 
         side,
         location,
         frequency, 
         stiffness,
         decrement,
         relaxation,
         creep)  %>%
  mutate_all(funs(tolower(.))) %>% 
  filter(pattern == "data collection 1" | 
           pattern == "data collection 2" | 
           pattern == "data collection 3") %>% 
  mutate(time = case_when(pattern == "data collection 1" ~ 1,
                   pattern == "data collection 2" ~ 2,
                   pattern == "data collection 3" ~ 3),
         object = factor(object)) %>% 
  mutate_at(vars(frequency:creep), funs(as.numeric)) %>% 
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>% 
  group_by(id, time)%>% 
  summarise_at(vars(frequency:creep), funs(sum)) %>% 
  arrange(id) %>% 
  ungroup()

## Prep data for asym calculation
asym <- read_excel("data/rawdata.xlsx",
                  sheet = "Myoton database", 
                  col_names = TRUE, 
                  na = c("-", "NA")) %>% 
  rename_all(funs(make.names(.))) %>%
  rename_all(funs(tolower(.))) %>% 
  select(measurement.time, 
         subject.name, 
         id, 
         dominant.side,
         gender,
         pattern, 
         object, 
         side,
         location,
         frequency, 
         stiffness,
         decrement,
         relaxation,
         creep)  %>%
  mutate_all(funs(tolower(.))) %>% 
  filter(pattern == "data collection 1" | 
           pattern == "data collection 2" | 
           pattern == "data collection 3") %>% 
  mutate(time = case_when(pattern == "data collection 1" ~ 1,
                          pattern == "data collection 2" ~ 2,
                          pattern == "data collection 3" ~ 3),
         object = factor(object)) %>% 
  mutate_at(vars(frequency:creep), funs(as.numeric)) %>% 
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>% 
  group_by(id, time)%>% 
  group_by(time, id) %>% 
  select(id, time, side, object, frequency:creep) 



left <- asym %>% 
    filter(side == "left") %>% 
  rename_at(vars(frequency:creep), funs(paste0(.,"_l"))) %>% 
  ungroup() %>% 
  mutate(idnew = 1:length(id))

right <- asym %>% 
  filter(side == "right") %>% 
  rename_at(vars(frequency:creep), funs(paste0(.,"_r"))) %>% 
  ungroup() %>% 
  mutate(idnew = 1:length(id)) %>% 
  select(-c(side, id, object, side, time))

## add other asym here

combined <- left_join(left, right, by = c("idnew")) %>% 
  group_by(id, time) %>% 
  summarise_at(vars(frequency_l:creep_l, frequency_r:creep_r), funs(sum)) %>% 
  rowwise() %>% 
  mutate(stiffness_asym = (stiffness_l-stiffness_r)/stiffness_l*100,
         stiffness_asym = abs(stiffness_asym)) %>% 
  ungroup() %>% 
  select(id, time, stiffness_asym)

## join back with main data

myoton <- left_join(myoton, combined, by = c("id", "time"))

rm(left, right, combined, asym)

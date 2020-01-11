### cortsiol

library(tidyverse)
library(readxl)

### gather complete results into a df
path <- "data/cortisol/cortisolprocessed.xlsx"
ranges <- list(cell_cols("A:I"))
sheets <- path %>% 
  excel_sheets() 

allsaliva <- map2_df(sheets, 
                     ranges, 
                     ~ read_excel(path, sheet = .x, range = .y), .id ="sheet")%>% 
  separate(well, 
           into = c("text", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  select(-text)

### extract tubeids from main file
d1 <- read_excel("data/cortisol/CSS-MG-001.xlsx",
                 col_names = TRUE )

d1 <- d1 %>% select(-c(2,6:7,11:17))
d1 <- d1 %>% drop_na(id)
d1 <- d1[order(d1$id),]

d1 <- d1 %>% mutate(tubeid = as.numeric(str_sub(unique, start = 4, end = 6)),
                    prepost = case_when(tubeid %% 2 == 1 ~ "Pre",
                                        !tubeid%% 2 == 1 ~ "Post"),
                    time = case_when(date == 42969 | date == 42997 ~ 1,
                                     date == 43073 | date ==43104 ~ 2,
                                     date == 43158 | date == 43200 ~ 3),
                    sport = case_when(id < 118 ~ "Rugby",
                                      id > 117 ~ "Football"))

d1$prepost <- factor(d1$prepost)
d1$date <- as.Date.numeric(d1$date, origin = "1899-12-30")

d1$date <- format(as.Date(d1$date), "%d/%m/%Y")


sumtot <- d1 %>% 
  select(time, id, prepost, tubeid) %>% 
  group_by(id, time)  %>% 
  rename("num" = tubeid) %>% 
  select(-prepost)


### filter just samples
joinsal <- allsaliva %>% 
  filter(well.cat == "Unknown")

### join master sheet with samples
fulllist <- read_excel("data/cortisol/CSS-MG-001.xlsx") %>% 
  select(id, unique) %>% 
  separate(unique, 
           into = c("code", "num"), 
           sep = "A") %>% 
  select(-code) %>% 
  inner_join(., 
             joinsal, 
             by = "num") %>% 
  mutate(num = as.numeric(num),
         state = ifelse(num %% 2 == 0, "post", "pre"))


### final tidy with time included

cortsiol <- left_join(fulllist, sumtot, by = c("id", "num")) %>% 
  select(id, time, state, mean.con) %>% 
  spread(state, mean.con) %>% 
  mutate(delta = post - pre) %>% 
  mutate(change = ifelse(delta > 0, "positive", "negative"))


## cvs

interassay <- allsaliva %>% 
  filter(as.numeric(num) > 900) %>% 
  select(sheet, num, mean.od) %>% 
  mutate(mean.od = as.numeric(mean.od)) %>% 
  mutate(lohi = rep(c("low", "high"), times = 7))%>% 
  group_by(lohi)%>% 
  summarise(cv = sd(mean.od)/mean(mean.od)*100)




intraassay <- fulllist %>% 
  select(id, num, well.od)%>% 
  separate(well.od, c("one", "two"), sep = ",") %>% 
  mutate_at(vars(one, two) , list(~as.numeric(.))) %>% 
  rowwise() %>% 
  mutate(mean = mean(c(one,two)),
         sd = sd(c(one,two)),
         cv = sd/mean*100) %>% 
  ungroup() 

save(interassay, intraassay, file = "datavars/cortreliability.rds")



rm(sumtot, d1, ranges, path, sheets, fulllist, joinsal)

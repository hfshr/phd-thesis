##3 HR data for football

library(readxl)
library(tidyverse)
library(lubridate)
library(qwraps2)

path <- "data/sporthr/rugbyHRdata.xlsx"

ranges <- list(cell_cols("A:B"))
sheets <- path %>% 
  excel_sheets() %>% 
  set_names()

rugby <- map2_df(sheets, 
                 ranges, 
                 ~ read_excel(path, sheet = .x, range = .y), .id ="sheet") %>% 
  mutate(id = as.numeric(sheet)) %>% 
  left_join(., parchar, by = c("id")) %>% 
  select(id, Time, bpm, age, sportg) 

path <- "data/sporthr/football HR.xlsx"

ranges <- list(cell_cols("A:B"))
sheets <- path %>% 
  excel_sheets() %>% 
  set_names()

football <- map2_df(sheets, 
                    ranges, 
                    ~ read_excel(path, sheet = .x, range = .y), .id ="sheet") %>% 
  mutate(id = as.numeric(sheet)) %>% 
  left_join(., parchar, by = c("id")) %>% 
  select(id, Time, bpm, age, sportg) 

sports <- rbind(rugby, football)  %>% 
  mutate(Time = as_datetime(Time)) %>% 
  separate(Time, c("date", "time"), sep = " ") %>% 
  select(-date) %>%
  drop_na(time) %>% 
  mutate(time = hms(time),
         time = period_to_seconds(time)) %>% 
  group_by(id) %>% 
  mutate(time = ifelse(id == 112, cumsum(c(0, rep(5, 291))), time)) %>% 
  mutate(maxhr = 220 - age,
         perc_hr = maxhr * 0.85, 
         max_time = max(time),
         max_hr = max(bpm, na.rm = T),
         avg_hr = mean(bpm, na.rm = T)) 


sport_new <- sports %>% 
  left_join(., sports %>% 
              group_by(id) %>% 
              slice(2) %>%
              select(1,2) %>% 
              `colnames<-`(c("id", "time_dec")), "id") 
  

sport_new <- sport_new  %>% 
  left_join(., sport_new %>% 
              filter(bpm > perc_hr) %>% 
              group_by(id) %>% 
              mutate(new_time = cumsum(time_dec)) %>% 
              filter(new_time == max(new_time)) %>% 
              select(id, new_time), "id") %>% 
  group_by(id) %>% 
  mutate(perc_time = new_time/max_time*100) %>% 
  slice(1) %>% 
  drop_na(perc_time) %>% 
  ungroup() %>% 
  group_by(sportg) %>% 
  summarise(maxhr = mean_sd(maxhr, denote_sd = "paren", digits = 0),
            avg_hr = mean_sd(avg_hr, denote_sd = "paren", digits = 0),
            perc_time = mean_sd(perc_time, denote_sd = "paren", digits = 2))

         


hr_data <- sport_new %>% 
  filter(sportg == "football") %>% 
  gather(M, value, -sportg) %>% 
  select(-sportg) %>% 
  cbind(., sport_new %>% 
          filter(sportg == "rugby") %>% 
          gather(M2, valuenew, -sportg) %>% 
          select(-sportg)) %>% 
  select(-3) %>% 
  separate(value, c("M", "SD"), sep = " ") %>% 
  mutate(SD = gsub("[^0-9.-]", "", SD)) %>% 
  separate(valuenew, c("Mm", "SDd"), sep = " ") %>% 
  mutate(SDd = gsub("[^0-9.-]", "", SDd)) %>% 
  mutate(var = c("max_hr", "avg_hr", "%above70")) %>% 
  select(var, M, SD, Mm, SDd) 


save(hr_data, file = "datavars/hr_data.rds")

perc_above <- sport_new %>% 
  filter(bpm > perc_hr) %>% 
  group_by(id) %>% 
  mutate(new_time = cumsum(time_dec)) %>% 
  filter(new_time == max(new_time))
  

fot <- c(120, 120, 90)
rug <- c(60, 90, 120)


newtib <- tibble(var = "total time (min)",
                 M = 110,
                 SD = 17,
                 Mm = 90,
                 SDd = 30
                 ) 
 
    
  
  mutate(maxtime = slice(n()))
  filter(id == 49)
  
  
maxtime <- rugby_new %>% 
  group_by(id) %>% 
  slice(n())



  group_by(id) %>% 
  summarise(mean = mean(bpm, na.rm = T))

names(rugby)
rugby <- rugby %>% 
  group_by(id) %>% 
  summarise(maxtime = max(duration))
  

rugby <- read_excel_allsheets(path)


path <- "Saliva/football HR.xlsx"

ranges <- list(cell_cols("A:B"))
sheets <- path %>% 
  excel_sheets() %>% 
  set_names()

football <- map2_df(sheets, 
                    ranges, 
                    ~ read_excel(path, sheet = .x, range = .y), .id ="sheet") %>% 
  mutate(id = as.numeric(sheet))%>% 
  group_by(id) %>% 
  summarise(mean = mean(bpm, na.rm = T),
            max = max(bpm, na.rm = T))




means <- map_df(rugby, mean(.x$bpm, na.rm = TRUE)) %>% 
  gather(id, bpm)



test <- flatten(test)

ids <- names(new)

means <- map(new, "bpm") %>% 
  summarise(mean = mean(., na.rm = TRUE))

test <- map(rugby, "Time") %>% 
  mutate(Time = as.numeric(Time))
  
 

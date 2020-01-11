#### Merge different data sources together ###

## collecting sources of data needed to be combined

source("rfiles/datacleanernew.R")
source("rfiles/myotondatabase.R")
source("rfiles/cortisolfinal.R")


hrv <- read_csv("data/hrv/hrvdata.csv",col_names = T) %>% 
  mutate(#rmssd = ifelse(artifacts > 5, NA, rmssd),
         rmssd = ifelse(rmssd > 400, NA, rmssd))
         #HF = ifelse(artifacts > 5, NA, HF))


## removing people who only attended one data collection session

?read_csv
complete <- dinj %>% 
  filter(count != 1) %>% 
  filter(variable != 2)  %>% 
  left_join(., hrv, by = c("id", "time")) %>% 
  left_join(., myoton, by = c("id", "time")) %>% 
  left_join(., cortsiol, by = c("id", "time")) %>% 
  mutate(days_missed = ifelse(is.na(days_missed), 0, days_missed))

## for people with two injuries at one time point

extrainj <- dinj %>% 
  filter(count != 1) %>% 
  filter(variable == 2) %>% 
  left_join(., hrv, by = c("id", "time")) %>% 
  left_join(., myoton, by = c("id", "time")) %>% 
  left_join(., cortsiol, by = c("id", "time")) %>% 
  mutate(days_missed = ifelse(is.na(days_missed), 0, days_missed))

complete <- rbind(complete,
                  extrainj) %>% 
  arrange(id, time, variable) %>% 
  select(-c(startdate.y, enddate, lastdate, firstdate))

write_csv(complete, "data/fulldatajoined.csv")

rm(extrainj, complete, hrv, cortsiol, d, dinj, myoton, allsaliva)



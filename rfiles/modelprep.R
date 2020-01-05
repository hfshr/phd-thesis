library(caret)
library(zoo)
library(tidyverse)

## read in merged data
data <- read_csv("data/fulldatajoined.csv") %>% 
  filter(time != 4)  # remove last time point as only questionnaire data is available

# Cleaning up explanatory variables

## all possible explanatory
explan <- data %>% 
  select(c(1,2,7,9:15,17,19,37:47))

# basic set of explanatory
basic.explan <- explan %>% 
  select(id, time, variable, gender, sportg, clevel, hours, pic) %>% 
  mutate(pic = factor(pic, labels = c("no injury", "injury")),
         clevel = ifelse(clevel == "notplaying", "club_university_county", clevel)) %>% 
  group_by(id) %>% 
  mutate(hours = na.locf(hours)) %>% 
  ungroup()

# impute missing data

# select variables of interest from full data set and join with explanatory variables
voi <- data %>% 
  select(id, time, variable, injured, 
         nle, ple, tle,
         st, nst, gt,
         ndt, dt,
         stiffness, frequency, decrement,
         rmssd, sdnn, meanHR, 
         bis, bas, fffs) %>% 
  left_join(basic.explan, ., by = c("id", "time", "variable")) %>% 
  mutate(gt = ifelse(gt == 0, NA, gt),
         bal_asym = abs(ndt-dt))

# additional variables that wont be imputed
ids <- data %>% 
  select(id, time, variable, change, delta, pre, post, days_missed) 

## check how many missing

missingcount <- tibble(var = c("hrv", "myoton"),
                       missing = c(map(voi, ~sum(is.na(.)))$rmssd, map(voi, ~sum(is.na(.)))$stiffness),
                       total = c(668, 668)) %>% 
  mutate(percdiff = missing/total*100)

map(voi, ~sum(is.na(.)))

missingtot <- voi %>% 
  select(id, rmssd, stiffness) %>% 
  mutate(rmssd = tidyr::replace_na(rmssd, 0),
         stiffness = tidyr::replace_na(stiffness, 0))

## extract missing data ids
missing <- voi[!complete.cases(voi), ]
missingid <- missing %>% 
  select(id, time, variable, rmssd, stiffness, sdnn, frequency)

## impute missing values using bag impute
library(caret)
?preProcess

prepro <- preProcess(voi[,c(4:length(voi))], "bagImpute")
datanew <- predict(prepro, voi[,c(4:length(voi))]) %>% 
  cbind(ids, .)

library(readxl)
lescacounts <- read_excel("~/Downloads/ProjectR/phdthesis/data/lesca/lescacounts.xlsx", sheet = "Sheet3")

datanew <- datanew %>% 
  inner_join(., lescacounts, by = c("id", "time")) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(negsev = nle/totneg,
         totsev = tle/sum(totneg, totpos)) %>% 
  mutate(negsev = ifelse(negsev == "NaN", 0, negsev),
         totsev = ifelse(totsev == "NaN", 0, totsev)) %>% 
  ungroup()

save(datanew, missingcount, missingtot, missingid, file = "datavars/dataprepro.rds") # maintain formatting
write_csv(datanew, "data/datanew.csv") #
## clean up

rm(ids, prepro, voi, basic.explan, explan, missing, lescacounts)




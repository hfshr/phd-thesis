# rst rel
library(tidyverse)
d <- read_csv("data/rst/51itemdata.csv", col_names = T) %>% 
  drop_na() %>% 
  select(-id) %>% 
  select(c(2:52, 1))


colnames(d)[45:51] <- str_replace(names(d[45:51]), "I", "IMP")

# to get character strings
fffs <- paste(names(d[grep('FFFS', colnames(d))]), collapse = "+")
bis <- paste(names(d[grep('BIS', colnames(d))]), collapse = "+")
ri <- paste(names(d[grep('RI', colnames(d))]), collapse = "+")
gdp <- paste(names(d[grep('GDP', colnames(d))]), collapse = "+")
rr <- paste(names(d[grep('RR', colnames(d))]), collapse = "+")
i <- paste(names(d[grep('IMP', colnames(d))]), collapse = "+")


model_fffs <- paste("fffs", fffs, sep = "=~")
model_bis <- paste("bis", bis, sep = "=~")

# full model with all subscales
fullmodel <- 'fffs=~V9_FFFS+V39_FFFS+V45_FFFS+V46_FFFS+V48_FFFS+V52_FFFS+V58_FFFS+V59_FFFS 
bis=~V1_BIS+V2_BIS+V6_BIS+V10_BIS+V17_BIS+V18_BIS+V21_BIS+V29_BIS+V33_BIS+V43_BIS+V47_BIS+V50_BIS+V56_BIS+V57_BIS+V60_BIS+V61_BIS+V63_BIS
ri=~V14_RI+V15_RI+V32_RI+V35_RI
gdp=~V5_GDP+V12_GDP+V20_GDP+V31_GDP+V41_GDP+V54_GDP+V65_GDP
rr=~V4_RR+V8_RR+V16_RR+V23_RR+V25_RR+V30_RR+V36_RR+V37_RR
i=~V22_IMP+V27_IMP+V28_IMP+V38_IMP+V40_IMP+V44_IMP+V51_IMP'

# example of cfa model
fit <- cfa(fullmodel, data = d)
sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(d[1:51])



rex <- function(x){
  re = 1 - x^2
  re
}
slx <- enframe(sl) %>% 
  rownames_to_column("id") %>% 
  mutate(re = map(.x = value, ~rex(.x))) %>% 
  mutate(re = as.numeric(re)) %>% 
  mutate(nnames = str_extract_all(name, "[A-Z]+", simplify = T)[,2]) %>% 
  group_by(nnames) %>% 
  summarise(final = sum(value)^2/(sum(value)^2 + sum(re)))

re <- 1 - sl^2
sum(sl)^2 / (sum(sl)^2 + sum(re))


save(slx, file = "datavars/rstreliability.rds")

library(semTools)
reliability(fit)

# loop to use with nested time
comprel <- function(d){
  fit <- cfa(fullmodel, data = d)
  sl <- standardizedSolution(fit)
  sl <- sl$est.std[sl$op == "=~"]
  names(sl) <- names(d[1:51])
  re <- 1 - sl^2
  sum(sl)^2 / (sum(sl)^2 + sum(re))
}

# result 1
looper <- d %>%
  group_by(time) %>%
  nest() %>%
  mutate(test = map(data, comprel)) %>%
  unnest(test, .drop = T) 

# comparison with semTOOLs package 
othercomprel <- function(d){
  fit <- cfa(fullmodel, data = d)
  reliability(fit) %>% as.data.frame() %>% 
    rownames_to_column("metric") %>% 
    as_tibble()
}
# result 2
looper2 <- d %>%
  group_by(time) %>%
  nest() %>%
  mutate(test = map(data, othercomprel)) %>%
  unnest(test, .drop = T) 




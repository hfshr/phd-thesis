source("rfiles/datacleanernew.R")
library(tidyverse)
library(papaja)
library(qwraps2)
library(knitr)
library(kableExtra)

students <- data.frame(stringsAsFactors=FALSE,
          id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
                 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
                 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,
                 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
                 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
                 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
                 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136,
                 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
                 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162,
                 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174,
                 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187,
                 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200,
                 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213,
                 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226,
                 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238,
                 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251,
                 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264,
                 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277,
                 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290,
                 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302,
                 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315,
                 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328,
                 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341,
                 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, NA),
     student = c("y", "n", "y", "y", "y", "y", "y", "y", "n", "n", "y", "y",
                 "y", "n", "y", "y", "y", "y", "y", "y", "y", "n", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "n", "y", "y",
                 "n", "y", "y", "n", "y", "n", "n", "n", "n", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "n",
                 "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n",
                 "n", "n", "n", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y",
                 "y", "y", "y", "y", "n", "n", "n", NA)
)


# Participant characteristics


parchar <- d %>% 
  filter(time == 1 & variable == 1) %>% 
  select(id, male, hours, height, weight, clevel, sportg, pic) %>% 
  mutate(age = c(23, 23, 20, 18, 33, 20, 21, 21, 25, 25, 21, 22, 26, 21, 19, 19, 19, 20, 25, 20, 21, 25, 20, 18, 20, 21, 20, 21, 21, 21, 19, 21, 19, 20, 20, 20, 20, 20, 20, 19, 19, 21, 20, 19, 20, 19, 20, 21, 20, 20, 20, 20, 19, 21, 19, 20, 20, 20, 20, 19, 20, 20, 20, 20, 20, 20, 20, 19, 19, 21, 19, 21, 19, 20, 21, 19, 19, 21, 20, 19, 20, 26, 21, 20, 20, 21, 19, 19, 18, 18, 20, 18, 18, 20, 19, 19, 19, 21, 19, 19, 20, 18, 18, 21, 19, 21, 19, 20, 20, 21, 18, 23, 20, 22, 24, 17, 18, 18, 24, 20, 20, 20, 20, 24, 26, 22, 23, 25, 19, 20, 21, 21, 25, 21, 24, 24, 20, 23, 21, 20, 18, 19, 20, 20, 19, 19, 19, 19, 21, 19, 21, 20, 21, 20, 20, 19, 19, 20, 19, 20, 18, 22, 20, 19, 19, 19, 20, 19, 21, 19, 19, 19, 19, 19, 19, 20, 21, 20, 19, 19, 19, 19, 19, 19, 19, 20, 20, 19, 19, 19, 19, 19, 19, 21, 19, 21, 19, 19, 20, 19, 19, 21, 19, 20, 21, 19, 20, 19, 24, 19, 20, 19, 21, 20, 19, 19, 19, 19, 20, 19, 19, 19, 19, 24, 19, 20, 19, 20, 20, 22, 19, 19, 23, 21, 20, 23, 19, 20, 20, 22, 19, 19, 19, 19, 20, 20, 19, 19, 21, 19, 19, 20, 21, 22, 20, 21, 20, 19, 19, 19, 21, 20, 20, 20, 20, 20, 19, 20, 20, 20, 20, 20, 21, 20, 19, 19, 20, 21, 20, 20, 20, 18, 26, 26, 20, 18, 19, 20, 20, 21, 19, 19, 47, 47, 47, 48, 46, 47, 47, 48, 46, 47, 49, 49, 46, 49, 49, 47, 48, 20, 20, 20, 20, 20, 18, 18, 20, 19, 20, 18, 18, 21, 19, 21, 20, 20, 27, 19, 21, 21, 19, 19, 19, 20, 19, 19, 19, 19, 19, 19, 23, 19, 21, 19, 19, 19, 20, 21, 40, 40, 35)) %>% 
  mutate(male = factor(male, labels = c("female", "male"))) %>% 
  rename(gender = "male") %>% 
  group_by(gender) %>% 
  mutate(clevel = factor(case_when(clevel == "notplaying" ~ "Recreational",
                            clevel == "national" | clevel == "international" ~ "National/International",
                            clevel == "club_university_county" ~ "University"), 
                         levels = c("Recreational", "University", "National/International"))) %>% 
  mutate(teamsind = factor(case_when(sportg == "other" | sportg == "athletics" | sportg == "gym" ~ "Individual",
                                     sportg == "basketball" | sportg == "cricket" | sportg == "hockey" |
                                       sportg == "football" | sportg == "netball" | 
                                       sportg == "otherteam" | sportg == "rugby" ~ "Team"))) %>% 
  left_join(., students, "id") %>% 
  ungroup()


clevel <- parchar %>% 
  group_by(gender) %>% 
  summarise(Recreational = n_perc0(clevel == "Recreational"),
            University = n_perc0(clevel == "University"),
            "National/International" = n_perc0(clevel == "National/International")) %>% 
  gather(name, value, -gender) %>% 
  spread(gender, value) %>% 
  ungroup() %>% 
  arrange(match(name, c("Recreational", "University", "National/International")))


npars <- parchar %>% 
  group_by(gender) %>% 
  summarise(Age = mean_sd(age, denote_sd = "paren", digits = 1),
            Height = mean_sd(height, denote_sd = "paren", digits = 1),
            Weight = mean_sd(weight, denote_sd = "paren", digits = 1),
            "Hours per week" = mean_sd(hours, denote_sd = "paren", digits = 1),
            "Student" = n_perc0(student == "y"),
            "Non-student" = n_perc0(student == "n"),
            "Injured" = n_perc0(pic == "1"),
            "Healthy" = n_perc0(pic == "0")) %>% 
  gather(name, value, -gender) %>% 
  spread(gender, value) %>% 
  rbind(., clevel) %>% 
  rename("Male (n = 231) " = "male",
         "Female (n = 120)" = "female") %>% 
  ungroup()

npars <- npars[c(1,3,8,4,2,5,7,6,9,10,11),]


print(npars)

apa_table(npars,
          stub_indents = list("Demographics" = c(1:4),
                              "Sustained an injury within the last six months" = c(5:6),
                              "Student status" = c(7:8),
                              "Competitive level" = c(9:11)
                              ))


## second study participants

age <- parchar %>% 
  select(id, age)
ids <- read_csv("data/fulldatajoined.csv") %>% 
  drop_na(change) %>% 
  select(id, time, sport)


pnums <- dinj %>% 
  filter(id %in% ids$id) %>% 
  filter(variable == 1) %>% 
  group_by(time) %>% 
  count()

data <- parchar %>% 
  filter(id %in% ids$id) %>% 
  select(id, age, height, weight, hours, pic) %>% 
  left_join(., ids, "id") %>% 
  mutate(sport = factor(ifelse(sport == "rugby union", "rugby", sport)))

data %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(sport) %>% 
  count()

parchar2 <- data %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(., students, "id") %>% 
  group_by(sport) %>% 
  summarise(Age = mean_sd(age, denote_sd = "paren", digits = 1),
            Height = mean_sd(height, denote_sd = "paren", digits = 1),
            Weight = mean_sd(weight, denote_sd = "paren", digits = 1),
            "Hours per week spent training" = mean_sd(hours, denote_sd = "paren", digits = 1),
            "Student" = n_perc0(student == "y"),
            "Non-student" = n_perc0(student == "n"),
            "Injured" = n_perc0(pic == "1"),
            "Healthy" = n_perc0(pic == "0"))%>% 
  gather(name, value, -sport) %>% 
  spread(sport, value) 

parchar2 <- parchar2[c(1,3,8,4,2,5,7,6),]

print(parchar2)

# Number of participant in study
parnumbers <- dinj %>% 
  mutate(time = factor(time)) %>% 
  filter(variable != 2) %>% 
  group_by(time) %>% 
  summarise(n = n()) %>% 
  ungroup()

# Injury characteristics

injcounts <- dinj %>% 
  filter(injured == 1) %>% 
  select(id, gender, typeofinjuryg, days_missed, acu_over, contact_non, bodypart)

toi <- injcounts %>% 
  select(id, gender, typeofinjuryg) %>% 
  group_by(gender, typeofinjuryg) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  spread(gender, n)
  
bp <- injcounts %>% 
  select(id, gender, bodypart) %>% 
  group_by(gender, bodypart) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  spread(gender, n) 
  
dinj %>% 
  filter(injured == 1) %>% 
  group_by(id) %>% 
  mutate(max = ifelse(days_missed == max(days_missed), 1, 0)) %>% 
  filter(max == 1) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(id, time, injtot, typeofinjuryg, acu_over, contact_non, bodypart)%>% 
  gather(key = var, value = score, typeofinjuryg, acu_over, contact_non, bodypart) %>% 
  ggplot(aes(score)) +
  geom_bar(stat = "count")  +
  facet_wrap(~var, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.background = element_blank())



## variable cut offs
nlesplitr <- function(timenum){
  datanet %>% 
    select(time, nlec) %>%
    filter(time == timenum) %>%
    group_by(time) %>% 
    mutate(lower = log(nlec +1)) %>% 
    mutate_at(vars(lower), 
              function(x) cut(x, breaks = c(min(x), median(x), max(x)), 
                              include.lowest = TRUE, ordered_result = TRUE)) %>% 
    map(~as.numeric(sub('.(.+),.+', '\\1', levels(.x)))[2]) %>% 
    as_tibble() %>% 
    select(-nlec)  %>% 
    mutate(var = paste("nlec_", timenum, sep = "")) %>% 
    cbind(., datanet %>% 
            filter(time == timenum) %>% 
            select(nlec) %>% 
            mutate(nlec = log(nlec +1)) %>% 
            map(~min(.)) %>% 
            as_tibble() %>% 
            gather(varr, min) %>% 
            select(-varr)) %>% 
    cbind(., datanet %>% 
            filter(time == timenum) %>% 
            select(nlec) %>% 
            mutate(nlec = log(nlec +1)) %>% 
            map(~max(.)) %>% 
            as_tibble() %>% 
            gather(varr, max) %>% 
            select(-varr)) %>% 
    mutate(max = round(max, digits = 2)) %>% 
    mutate(Low = paste(min, lower, sep = "-"),
           High = paste(">", lower, sep = ""),
           High = paste(High, max, sep = "-")) %>% 
    mutate(Definition = "") %>% 
    select(var, Definition, Low, High) %>% 
    rename(state_1 = "Low", 
           state_2 = "High")
  
}
tlesplitr <-  function(timenum){
  datanet %>% 
    select(time, tlec) %>%
    filter(time == timenum) %>%
    group_by(time) %>% 
    mutate(lower = log(tlec)) %>% 
    mutate_at(vars(lower), 
              function(x) cut(x, breaks = c(min(x), median(x), max(x)), 
                              include.lowest = TRUE, ordered_result = TRUE)) %>% 
    map(~as.numeric(sub('.(.+),.+', '\\1', levels(.x)))[2]) %>% 
    as_tibble() %>% 
    select(-tlec)  %>% 
    mutate(var = paste("tlec_", timenum, sep = "")) %>% 
    cbind(., datanet %>% 
            filter(time == timenum) %>% 
            select(tlec) %>% 
            mutate(tlec = log(tlec)) %>% 
            map(~min(.)) %>% 
            as_tibble() %>% 
            gather(varr, min) %>% 
            select(-varr)) %>% 
    cbind(., datanet %>% 
            filter(time == timenum) %>% 
            select(tlec) %>% 
            mutate(tlec = log(tlec)) %>% 
            map(~max(.)) %>% 
            as_tibble() %>% 
            gather(varr, max) %>% 
            select(-varr)) %>% 
    mutate(max = round(max, digits = 2),
           min = round(min, digits = 2)) %>% 
    mutate(Low = paste(min, lower, sep = "-"),
           High = paste(">", lower, sep = ""),
           High = paste(High, max, sep = "-")) %>% 
    mutate(Definition = "") %>% 
    select(var, Definition, Low, High) %>% 
    rename(state_1 = "Low", 
           state_2 = "High") 
} 

names <- datanet %>% 
  select(nlebase, stiffness:FFFS) %>% 
  names()

varcutoffs <- tallc %>% 
  select(pi:hours)%>%
  select(-nlebase) %>% 
  map(~levels(.)) %>% 
  as_tibble() %>% 
  gather(var, state) %>% 
  mutate(ind = rep(c("state_1", "state_2"), length.out = n())) %>% 
  spread(ind, state)%>% 
  mutate(Definition = c("Current competitive level",
                        "Gender of the participant",
                        "Number of hours spent training per week",
                        "Participate in an individual or team based sport",
                        "Previous injury - Whether an injured had been sustained in the previous 12 months prior to the study")) %>% 
  mutate_at(vars(state_1, state_2), funs(str_to_title(.))) %>% 
  select(var, Definition, state_1, state_2) %>% 
  rbind(., datanet %>% 
          select(id, time, injured, pi, nlebase, gender, ind_team, clevel, hours, stiffness, 
                 RI, RR, I, GDP, BIS, rmssd, sdnn, bal_asym, balance, FFFS) %>% 
          mutate_at(vars(stiffness:FFFS, nlebase), 
                    function(x) cut(x, breaks = c(min(x), median(x), max(x)), 
                                    include.lowest = TRUE, ordered_result = TRUE)) %>% 
          select(stiffness:FFFS, nlebase) %>% 
          map(~as.numeric(sub('.(.+),.+', '\\1', levels(.x)))[2]) %>% 
          as_tibble()%>%  
          gather(var, lower) %>% 
          {. ->> namess} %>% 
          cbind(., datanet %>% 
                  select(namess$var) %>% 
                  map(~min(.)) %>% 
                  as_tibble() %>% 
                  gather(varr, min) %>% 
                  select(-varr)) %>% 
          cbind(., datanet %>% 
                  select(namess$var) %>% 
                  map(~max(.)) %>% 
                  as_tibble() %>% 
                  gather(varr, max) %>% 
                  select(-varr)) %>% 
          mutate(Low = paste(min, lower, sep = "-"),
                 High = paste(">", lower, sep = ""),
                 High = paste(High, max, sep = "-")) %>% 
          select(var, Low, High) %>% 
          mutate(var = factor(var, levels = c("nlebase", "FFFS", "BIS", "RI",
                                              "RR", "I", "GDP", "stiffness", "rmssd",
                                              "sdnn", "bal_asym", "balance"))) %>% 
          arrange(var) %>% 
          mutate(Definition = c("Baseline NLE",
                                "Fight-Flight-Freeze System",
                                "Behavioural Inhibition System",
                                "Reward Interest",
                                "Reward reactivity",
                                "Impulsivity",
                                "Goal drive persistence",
                                "Sum of all stiffness locations",
                                "Root mean squared difference of successive RR intervals",
                                "Standard deviation of HRV",
                                "Percentage difference between left and right leg balance score",
                                "Total balance score")) %>% 
          rename(state_1 = "Low", 
                 state_2 = "High") %>% 
          select(var, Definition, state_1, state_2))%>% 
  rbind(., map_df(1:3, nlesplitr)) %>% 
  rbind(., map_df(1:3, tlesplitr)) %>% 
  rowid_to_column("tempid") %>% 
  mutate(state_1 = ifelse(tempid > 5, paste(state_1, "(Low)"), state_1),
         state_2 = ifelse(tempid > 5, paste(state_2, "(High)"), state_2),
         state_1 = ifelse(tempid == 3 ,"0-9 (Low)", state_1),
         state_2 = ifelse(tempid == 3, ">9-35 (High)", state_2)) %>% 
  select(-tempid)

varcutoffs[18,2] <- "Log nle at time point 1"
varcutoffs[19,2] <- "Log nle at time point 2"
varcutoffs[20,2] <- "Log nle at time point 3"
varcutoffs[21,2] <- "Log tle at time point 1"
varcutoffs[22,2] <- "Log tle at time point 2"
varcutoffs[23,2] <- "Log tle at time point 3"

save(npars, parchar, parnumbers, parchar2, file = paste0("datavars/parchar", ".RData"))
save(varcutoffs, file = "datavars/dataanalysismethods.rds")

load("datavars/parchar.RData")


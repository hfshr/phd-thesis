fulllist <- read_csv("data/rst/fulllist.csv")
shortlist <- tibble(item = c("I regularly try new activities just to see if I enjoy them.", "I get carried away by new projects.", "I’m always finding new and interesting things to do.", "I am very open to new experiences in life.", "I put in a big effort to accomplish important goals in my life.", "I’m motivated to be successful in my personal life.", "I often overcome hurdles to achieve my ambitions.", "I feel driven to succeed in my chosen career.", "I am very persistent in achieving my goals.", "I think it is necessary to make plans in order to get what you want in life.", "I will actively put plans in place to accomplish goals in my life.", "I am especially sensitive to reward.", "I often experience a surge of pleasure running through my body.", "Good news makes me feel over-joyed.", "I often feel that I am on an emotional ‘high’.", "I get a special thrill when I am praised for something I’ve done well.", "I get very excited when I get what I want.", "I always celebrate when I accomplish something important.", "I find myself reacting strongly to pleasurable things in life.", "I think I should ‘stop and think’ more instead of jumping into things too quickly.", "I sometimes cannot stop myself talking when I know I should keep my mouth closed.", "I often do risky things without thinking of the consequences.", "I find myself doing things on the spur of the moment.", "I’m always buying things on impulse.", "I would go on a holiday at the last minute.", "I think the best nights out are unplanned.", "I feel sad when I suffer even minor setbacks.", "I am often preoccupied with unpleasant thoughts.", "I sometimes feel ‘blue’ for no good reason.", "I have often spent a lot of time on my own to “get away from it all”.", "The thought of mistakes in my work worries me.", "When nervous, I sometimes find my thoughts are interrupted.", "I often feel depressed.", "My mind is sometimes dominated by thoughts of the bad things I’ve done.", "I’m always weighing-up the risk of bad things happening in my life.", "I often worry about letting down other people.", "I worry a lot.", "It’s difficult to get some things out of my mind.", "I find myself thinking about the same thing over and over again.", "I often wake up with many thoughts running through my mind.", "I often find myself ‘going into my shell’.", "My mind is dominated by recurring thoughts.", "I take a long time to make decisions.", "I would be frozen to the spot by the sight of a snake or spider.", "I would instantly freeze if I opened the door to find a stranger in the house.", "I would run fast if I knew someone was following me late at night.", "I would leave the park if I saw a group of dogs running around barking at people.", "I would freeze if I was on a turbulent aircraft.", "There are some things that I simply cannot go near.", "I would not hold a snake or spider.", "Looking down from a great height makes me freeze."))

## code to filter questions from tom
library(fuzzyjoin)

## match mine and toms questions + identify sub scales
test <- fulllist %>% 
  stringdist_left_join(shortlist, by = c("item")) %>% 
  filter(!is.na(item.y)) %>% 
  mutate(numberc = str_c("V", number, sep = "")) %>% 
  mutate(label = case_when(number %in% c(9, 19, 39, 45, 46, 48, 52, 58, 59, 62) ~ "FFFS",
                           number %in% c(1, 2, 6, 7, 10, 17, 18, 21, 29, 33, 34, 42, 43, 47, 
                                         49, 50, 55, 56, 57, 60, 61, 63, 64) ~ "BIS",
                           number %in% c(11, 13, 14, 15, 26, 32, 35) ~ "RI",
                           number %in% c(5, 12, 20, 31, 41, 54, 65) ~ "GDP",
                           number %in% c(3, 4, 8, 16, 23, 24, 25, 30, 36, 37) ~ "RR" ,
                           number %in% c(22, 27, 28, 38, 40, 44, 51, 53 ) ~ "I"))

# arrange colums grouped into sub scales  
lab <- test %>% 
  select(numberc, label) %>% 
  mutate(label = factor(label, levels = c("FFFS", "BIS", "RI", "GDP", "RR", "I"))) %>% 
  arrange(as.numeric(label)) %>% 
  unite(fullname, c(numberc, label), sep = "", remove = F)


neworder <- lab %>% 
  unite(questioncode, c(numberc, label), sep = "_")

## 51 item 
rst <- read_csv("data/rst/fulllongdata.csv")%>% 
  gather(question, response, -id, -time) %>% 
  filter(question %in% test$numberc) %>% 
  spread(question, response) %>% 
  select(id, time, lab$numberc) %>% 
  rename_at(vars(3:53), list(~lab$fullname))

write_csv(rst, "data/rst/51itemdata.csv")
## summarise 51 item per id per time
x <- lab$fullname

finalrst <- read_csv("data/rst/51itemdata.csv") %>% 
  drop_na() %>% 
  gather(question, response, -id, -time) %>% 
  mutate(var = case_when(question %in% x[c(1:8)] ~ "FFFS",
                         question %in% x[c(9:25)] ~ "BIS",
                         question %in% x[c(26:29)] ~ "RI",
                         question %in% x[c(30:36)] ~ "GDP",
                         question %in% x[c(37:44)] ~ "RR",
                         question %in% x[c(45:51)] ~ "I")) %>% 
  group_by(id, time, var)%>% 
  summarise(sum = sum(response)) %>% 
  spread(var, sum) %>% 
  ungroup() %>% 
  mutate(BAS = rowSums(.[5:8])) %>% 
  select(id, time, FFFS, BIS, BAS, everything())


save(finalrst, file = "datavars/finalrst.rds")



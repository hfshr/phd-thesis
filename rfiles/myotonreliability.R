library(tidyverse)
library(readxl)

## read and clean data
mm <- data.frame(
  diff = rep(c(11, 9, 6, 7, 9, 6, 8, 4, 11, 2, 9, 19, 6, 7, 6, 16, 9, 13, 17,
           14, 6, 2, 12, 10, 9, 6, 5, 13, 9, 9, 10, 2, 6, 1, 7, 3, 7, 5,
           5, 10), each = 3),
  id = rep(c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1002, 1002,
         1002, 1002, 1002, 1002, 1002, 1002, 1004, 1004, 1004, 1004,
         1004, 1004, 1004, 1004, 1001, 1001, 1001, 1001, 1001, 1001, 1001,
         1001, 1003, 1003, 1003, 1003, 1003, 1003, 1003, 1003), each = 3)
) %>% 
  arrange(id) %>% 
  select(-id)

hw <- read_excel("data/rawdata.xlsx",
                           sheet = "Myoton database", 
                           col_names = TRUE, 
                           na = c("-", "NA")) %>% 
  rename_all(funs(make.names(.))) %>%
  rename_all(funs(tolower(.))) %>% 
  select(id, height, weight) %>% 
  filter(id > 900 & id < 1020) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(age = c(26, 28, 23, 25, 26))




pilot  <- read_excel("data/rawdata.xlsx",
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
         weight,
         height,
         location,
         frequency, 
         stiffness,
         decrement,
         relaxation,
         creep)  %>%
  mutate_all(funs(tolower(.))) %>% 
  filter(pattern == "set 1" | pattern == "set 2" ) %>%
  mutate(set = ifelse(pattern == "set 1", 1, 2),
         object = factor(object)) %>% 
  mutate_at(vars(frequency:creep), list(~as.numeric)) %>% 
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>% 
  select(id, subject.name, set, object, side, stiffness, frequency, decrement) %>% 
  mutate(object = case_when(object == "bic fem c l" ~ "bf",
                            object == "gastr c lt" ~ "lg",
                            object == "gastr c m" ~ "mg",
                            object == "rect femoris" ~ "rf"),
         object = factor(object, levels = c("rf", "bf", "mg", "lg")),
         side = factor(side)) %>% 
  arrange(id, set, object, side)%>% 
  gather(variable, value, -id, -subject.name, -set, -object, -side) %>% 
  spread(set, value) %>% 
  cbind(., mm) %>% 
  rename("one" = "1",
         "two" = "2")%>% 
  rowwise() %>% 
  mutate(valuediff = abs(two - one),
         percdiff = abs((one - two)/one*100)) %>% 
  ungroup()%>% 
  #gather(., "set", "value", -id, -variable, -subject.name, -object, -side, -diff, -valuediff) %>% 
  arrange(id, variable)


## Bayesian difference
library(brms)
library(BEST)
library(broom)

# function for nested t tests
t_test <- function(pilot, paired = T, conf.level = .95) {
  test <- bayes.t.test(pilot$one, 
              pilot$two,
              paired = paired,
              conf.level = conf.level)
  set <- test$stats %>% as.data.frame() %>% 
    rownames_to_column(var = "parameter")
}

ptt <- pilot %>%
  group_by(variable, object) %>%
  nest() %>%
  mutate(ttest = map(data, t_test)) %>%
  unnest(ttest, .drop = T) 

load("datavars/myorel.Rdata")

final <- ptt %>% 
  select(1:11) %>% 
  filter(parameter == "mu_diff") %>% 
  select(-c(3,7)) %>% 
  rename("pr > 0" = "%>comp",
         "pr < 0" = "%<comp") %>% 
  rename("lower" = "HDIlo",
         "upper" = "HDIup",
         "width" = "HDI%") %>% 
  mutate(conf.low = lower,
         conf.high = upper) %>% 
  mutate_at(vars(conf.low, conf.high), list(~format(round(. ,2), nsmall = 2))) %>%
  mutate(conf.low = str_c("[", conf.low, sep = ""),
         conf.high = str_c(conf.high, "]", sep = "")) %>% 
  unite(., "95% Cl", c("conf.low", "conf.high"), sep = ", ") %>%
  mutate("width" = 0.95)

library(tidybayes)
library(papaja)
scales_x <- list(
  decrement = scale_x_continuous(limits = c(-0.15, 0.2)),
  frequency = scale_x_continuous(limits = c(-0.5, 0.5)),
  stiffness = scale_x_continuous(limits = c(-13, 14)))

final %>% 
  ggplot(., aes(y = object, x = mean, xmin =lower, xmax = upper, label = `95% Cl`)) +
  ggstance::geom_pointrangeh() +
  geom_vline(xintercept = 0) +
  geom_text(aes(x = 0), hjust = -0.8, vjust = 1.5, size = 3) +
  facetscales::facet_grid_sc(cols = vars(variable), scales = list(x = scales_x)) +
  theme_apa() +
  theme(panel.spacing.x = unit(0.7, "lines")) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
  

 
## function for nested bayes factors
library(BayesFactor)  

bf_test <- function(pilot, paired = T){
  test <- ttestBF(pilot$one, 
                  pilot$two,
                  paired = paired) 
  set1 <- tibble(bf = test@bayesFactor[["bf"]],
                 error = test@bayesFactor[["error"]])
}

bf_test(pilot)

bayesttest <- pilot %>%
  group_by(variable, object) %>%
  nest() %>%
  mutate(ttest = map(data, bf_test)) %>%
  unnest(ttest, .drop = T)




  # %>% 
  #   mutate_at(vars(HDIlo, HDIup), list(~format(round(. ,2), nsmall = 2))) %>%
  #   mutate(HDIlo = str_c("[", HDIlo , sep = ""),
  #          HDIup = str_c(HDIup, "]", sep = "")) %>% 
  #   unite(., "95% Cl", c("HDIlo", "HDIup"), sep = ", ") %>% 
  
## measurememnt error
differences <- pilot %>% 
  group_by(side, object) %>% 
  summarise(min = min(diff),
            max = max(diff),
            mean = mean(diff),
            sd = sd(diff)) %>% 
  gather(measure, value, -side, -object) %>% 
  spread(measure, value) %>% 
  select(side, object, min, max, mean, sd)%>% 
  {. ->> temp} %>% 
  filter(side == "left") %>% 
  cbind(., temp[5:8,]) %>% 
  ungroup() %>% 
  select(-side1, -object1, -side1, -side) 

save(hw, differences, final, ptt, file = "datavars/myorel.Rdata")
  


### junk

fit1
summary(fit1)
### replace standard pair t-tests with 
library(BayesianFirstAid)

fit1$stats

fit1 <- bayes.t.test(pilot$one, pilot$two, paired = T, alternative = "two.sided")
summary(fit1)
plot(fit1)
diagnostics(fit1)
x <- as_tibble(fit1$stats, rownames = NA)


 


library(papaja)
library(ggpubr)
library(broom)

?bayes.cor.test

library(BayesFactor)
bf = ttestBF(x = pilot$one, y = pilot$two, paired=TRUE)
bf

## loop for many t tests
library(broom)
t_test <- function(pilot, mu = 0, alt = "two.sided", paired = T, conf.level = .99) {
  tidy(t.test(pilot$one, 
              pilot$two,
              mu = mu, 
              alt = alt,
              paired = paired,
              conf.level = conf.level))
}
library(data.table)
ptt <- pilot %>%
  group_by(variable, object) %>%
  nest() %>%
  mutate(ttest = map(data, t_test)) %>%
  unnest(ttest, .drop = T) %>% 
  select(-method, -alternative) %>% 
  rename(Characteristic = "variable")

test <- pilot %>% 
  select(variable, valuediff, diff) %>% 
  nest(-variable) %>% 
  mutate(test = map(data, ~ cor.test(.x$valuediff, .x$diff)),
         tidied = map(test, tidy)) %>% 
  unnest(tidied, .drop = TRUE) %>% 
  select(variable, "p.value", estimate) %>%
  mutate(p.value = as.character(format(round(p.value, digits = 2), nsmall = 2)),
         estimate = as.character(format(round(estimate, digits = 2), nsmall = 2))) %>% 
  mutate(x = c(0.11, 0.3, 12),
         y = c(20, 20, 20)) %>% 
  mutate(p.label = paste0("p = ", p.value),
       r.label = paste0("R = ", estimate),
       label = paste0("italic(R)==", estimate, ",~~italic(p)==", p.value))  



plotcor <- pilot %>% 
  left_join(., test, by = "variable") %>% 
  mutate(variable = fct_recode(variable, `Stiffness (N/m)` = "stiffness", 
                               `Tone (Hz)`= "frequency", 
                               Elasticity = "decrement"),
         variable = factor(variable, levels = c("Tone (Hz)", 
                                                "Stiffness (N/m)", 
                                                "Elasticity")))

fit2 <- bayes.cor.test(~ diff + percdiff, data = pilot)

summary(fit2)
plot(fit2)
diagnostics(fit2)

fit3 <- bayes.cor.test(~ diff + valuediff, data = plotcor[plotcor$variable == "Elasticity", ])
summary(fit3)
plot(fit3)

?bayes.cor.test
plotcor %>% 
  ggplot(aes(valuediff, diff)) +
  geom_point() +
  ggpubr::stat_cor(method = "spearman", label.y = 20, label.x.npc = 0.1) + 
  geom_smooth(method = "lm", se = F, colour = "black", size = 0.7) +
  facet_wrap(variable~., scales = "free") +
  ylab("measurement error (mm)") +
  xlab("Absolute difference between set one and two") +
  papaja::theme_apa()

  ggsave("corplot.png", width = 20, height = 10, units = "cm")
  





geom_text(aes(x, y, aes(label = label, group = NULL), data = test, parse = TRUE))
paste("~italic(R)==~", estimate, "~`,`~", "~italic(p)==", p.value),
save(differences, plot, ptt, file = "datavars/myorel.RData")




mtcars[, c("cyl", "am", "gear")] <- lapply(mtcars[, c("cyl", "am", "gear")], as.factor)

p <- ggplot(mtcars, aes(mpg, wt, group = cyl)) + 
  geom_line(aes(color=cyl)) +
  geom_point(aes(shape=cyl)) + 
  facet_grid(gear ~ am) +
  theme_bw()                                                                      
p 

shapiro.test(pilot$stiffness)

res <- t.test(stiffness ~ set, paired = TRUE, data = pilot)
res

summary(lm(diff ~ abs(stiffdiff), data = pilot))

cor(abs(pilot$stiffdiff), pilot$diff)

ggplot(pilot, aes(abs(stiffdiff), diff)) +
  geom_point() +
  geom_smooth(method = "lm")




  

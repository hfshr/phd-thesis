# This script first combines results from a custom r script an kubios software.
# Robust bayesian correlations are then performed using brms
load(file = "datavars/hrvvcomparison.rds")

rcode <- read_csv(file = "data/hrv/hrvdata.csv", col_names =T) %>% 
  filter(time == 1 & id < 21) %>% 
  select(c(1,3,5,4)) %>% 
  mutate(method = "rcode")

names <- names(rcode)

files <- dir("data/hrv/kubiostxt/", pattern = "*.csv")

filesnames <- tibble(filenames = str_remove(files, 3))

readfunc <- function(file){
  data <- read.csv(paste0("data/hrv/kubiostxt/",file), 
                   header = F, 
                   skip = 50, nrows = 20, 
                   stringsAsFactors = FALSE)
  data <- data[c(5,6,10),c(1:2)]
  data
}

data <- files %>% 
  map(readfunc) %>% 
  reduce(rbind) %>% 
  mutate(V2 = as.numeric(V2))%>% 
  as_tibble() %>% 
  mutate(id = rep(c(1:20), each = 3)) %>% 
  spread(V1, V2) %>% 
  select(id, everything()) %>% 
  janitor::clean_names() %>% 
  mutate(method = "kubios") %>% 
  `names<-`(names)

alldata <- left_join(rcode, data, "id", suffix = c("R", "K"))

plot(alldata$meanHR_rcode ~ alldata$meanHR_kubios)
plot(alldata$rmssd_rcode ~ alldata$rmssd_kubios)

#write final results to file
write_csv(alldata, "kubiocomparison.csv")

# testing BF using Bayesfactor package
library(papaja)
library(BayesFactor)
result1 <- ttestBF(alldata$rmssdR, alldata$rmssdK, paired = T)
result2 <- ttestBF(alldata$sdnnR, alldata$sdnnK, paired = T)

apa_print.BFBayesFactor(result)$statistic
apa_print.BFBayesFactor(result2)

library(brms)

# below these model do a cprrelation between each variable. 
# I could have run one model with a grouping factor but didn't have the data in the right format,
# So did it the brite force way.
# results then combined in a table for each value

f2 <- 
  brm(data = alldata, 
      family = student,
      mvbind(meanHRR, meanHRK) ~ 1,
      prior = c(prior(gamma(2, .1), class = nu),
                prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = sigma, resp = meanHRR),
                prior(normal(0, 100), class = sigma, resp = meanHRK),
                prior(lkj(1), class = rescor)),
      iter = 2000, warmup = 500, chains = 4, cores = 4)

f3 <- 
  brm(data = alldata, 
      family = student,
      mvbind(rmssdR, rmssdK) ~ 1,
      prior = c(prior(gamma(2, .1), class = nu),
                prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = sigma, resp = rmssdR),
                prior(normal(0, 100), class = sigma, resp = rmssdK),
                prior(lkj(1), class = rescor)),
      iter = 2000, warmup = 500, chains = 4, cores = 4) ; beep(7)

# Commenetd as used to try bayes_factor function in brms. Save_all_aprs needs to be included. 
# f3.1 <- update(f3, save_all_pars = T)
# f3.2 <- 
#   brm(data = alldata, 
#       family = student,
#       mvbind(rmssdR, rmssdK) ~ 0,
#       prior = c(prior(gamma(2, .1), class = nu),
#                 prior(normal(0, 100), class = sigma, resp = rmssdR),
#                 prior(normal(0, 100), class = sigma, resp = rmssdK),
#                 prior(lkj(1), class = rescor)),
#       iter = 2000, warmup = 500, chains = 4, cores = 4, save_all_pars = T) ; beep(7)

f4 <- 
  brm(data = alldata, 
      family = student,
      mvbind(sdnnR, sdnnK) ~ 1,
      prior = c(prior(gamma(2, .1), class = nu),
                prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = sigma, resp = sdnnR),
                prior(normal(0, 100), class = sigma, resp = sdnnK),
                prior(lkj(1), class = rescor)),
      iter = 2000, warmup = 500, chains = 4, cores = 4) 

# f4.1 <- update(f4, save_all_pars = T)
# f4.2 <- brm(data = alldata, 
#             family = student,
#             mvbind(sdnnR, sdnnK) ~ 0,
#             prior = c(prior(gamma(2, .1), class = nu),
#                       prior(normal(0, 100), class = sigma, resp = sdnnR),
#                       prior(normal(0, 100), class = sigma, resp = sdnnK),
#                       prior(lkj(1), class = rescor)),
#             iter = 2000, warmup = 500, chains = 4, cores = 4, save_all_pars = T) 


# bayes_factor(f4.1, f4.2, log = T)
save(f2, f3, f4, hrvcomp, file = "datavars/hrvvcomparison.rds")

# posible options for combining results
# final <- tidy(f3, parameters = "^re")  %>% 
#   rbind(., tidy(f4, parameters = "^re")) %>% 
#   rbind(., tidy(f2, parameters = "^re")) %>% 
#   mutate(variable = c("rmssd", "sdnn", "meanHR")) %>% 
#   select(variable, everything()) %>% 
#   select(-term, -component)

hrvcomp <- summary(f4)$rescor %>% # went with this option
  as.data.frame() %>% 
  rbind(., summary(f3)$rescor %>% as.data.frame()) %>% 
  rbind(., summary(f2)$rescor %>% as.data.frame()) %>% 
  mutate(variable = c("sdnn", "rmssd", "mean HR")) %>% 
  select(variable, everything()) %>% 
  select(-Eff.Sample)

# tab <- tidy_stan(f4)[4,] %>% 
#   rbind(., tidy_stan(f3)[4,]) %>% 
#   rbind(., tidy_stan(f2)[4,]) %>% 
#   select(-term)

# possible plotting options
posts <- f2 %>% 
  gather_draws(rescor__meanHRR__meanHRK) %>% 
  rbind(., f3 %>% 
          gather_draws(rescor__rmssdR__rmssdK)) %>% 
  rbind(., f4 %>% 
          gather_draws(rescor__sdnnR__sdnnK)) 

head(posts)
posts %>% 
  ggplot(aes(x=.value)) +
  geom_density() +
  facet_wrap(~.variable, scales = "free") +
  theme_apa()+
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


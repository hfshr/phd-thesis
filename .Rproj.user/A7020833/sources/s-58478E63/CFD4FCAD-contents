## Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, tidyverse, ggpubr, tidybayes, broom, ggeffects, sjstats, sjmisc)

## Functions

### graphmaker - used for creating plots in manuscript

graphmaker <- function(x, var) {
  if (var == "delta_new_z:sportg") {
    data <- x[[var]]
    cols <- c("football" = "red", "rugby" = "blue")
    p <- data %>%
      ggplot(aes_string(names(data)[1], "estimate__", group = "sportg")) +
      geom_line() +
      geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = sportg), alpha = 0.3) +
      scale_fill_manual(values = cols) +
      scale_alpha(guide = "none")
  }
  else {
    data <- x[[var]]
    p <- data %>%
      ggplot(aes_string(names(data)[1], "estimate__")) +
      geom_line() +
      geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.2)) +
      scale_alpha(guide = "none")
  }
  return(p)
}

## Data cleaning / prep

data <- read_csv("data/datanew.csv") %>%
  drop_na(change) %>%
  mutate(
    time = as.ordered(time),
    clevel = as.ordered(clevel),
    injured = factor(injured, labels = c("healthy", "injured")),
    sportg = as.factor(sportg),
    pic = as.factor(pic),
    change = as.factor(change),
    nlem = log10(nle + 1)
  ) %>%
  rename("balance" = gt)

set1 <- data %>%
  select(
    id,
    time,
    sportg,
    injured,
    delta,
    nle,
    pre,
    post,
    negsev,
    fffs,
    bis,
    bas,
    stiffness,
    rmssd,
    totneg,
    days_missed
  ) %>%
  group_by(id) %>%
  mutate(
    nlec = cumsum(nle),
    injtot = sum(ifelse(injured == "injured", 1, 0))
  ) %>%
  ungroup() %>%
  mutate(idx = as.numeric(factor(id))) %>%
  group_by(id) %>%
  mutate(
    injured = ifelse(injured == "healthy", 0, 1),
    injcount = cumsum(injured),
    injtot = sum(injured),
    oneinj = ifelse(injtot > 0, 1, 0),
    injfac = ifelse(injured == 1, "injured", "healthy"),
    injcountfact = case_when(
      injcount == 0 ~ 1,
      injcount == 1 ~ 2,
      injcount > 1 ~ 3
    ),
    deltag = if_else(delta > 0, 1, 0),
    totnegc = cumsum(totneg),
    days_missed_tot = cumsum(days_missed)
  ) %>%
  ungroup() %>%
  group_by(time) %>%
  mutate(nleg = if_else(nle > mean(nle), "High", "Low")) %>%
  ungroup() %>%
  mutate(days_missed_code = factor(
    case_when(
      days_missed == 0 ~ "healthy",
      days_missed < 30 ~ "minor",
      days_missed > 29 ~ "major"
    ),
    levels = c("healthy", "minor", "major"),
    ordered = TRUE
  )) %>%
  mutate(days_missed_code_tot = factor(
    case_when(
      days_missed_tot == 0 ~ "healthy",
      days_missed_tot < 30 ~ "minor",
      days_missed_tot > 29 ~ "major"
    ),
    levels = c("healthy", "minor", "major"),
    ordered = TRUE
  ))

## Final data prep for use in models

sem <- set1 %>%
  select(
    id,
    time,
    negsev,
    nlec,
    nle,
    stiffness,
    delta,
    injured,
    injcount,
    days_missed,
    sportg,
    fffs,
    pre,
    post
  ) %>%
  mutate(injcount = ifelse(injcount == 3, 2, injcount)) %>%
  group_by(id, time) %>%
  mutate(delta_new = post / (post + pre)) %>%
  ungroup() %>%
  std(negsev, delta, stiffness, delta_new)

write_csv(sem, "data/study2_cleandata.csv")

## Read in data

sem <- read_csv("data/study2_cleandata.csv")

## First model

options(mc.cores = parallel::detectCores())

prior1 <- c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0,2.5)", class = "b"),
  set_prior("cauchy(0,1)", class = "sd")
)

fit1 <- brm(
  bf(
    days_missed ~ negsev_z + delta_new_z:sportg + stiffness_z + (1 | id),
    hu ~ negsev_z + delta_new_z:sportg + stiffness_z + (1 | id)
  ),
  prior = prior1,
  data = sem,
  family = hurdle_negbinomial(),
  control = list(adapt_delta = 0.99)
)


summary(fit1)

tidy_stan(fit1)

launch_shinystan(fit1)

x <- marginal_effects(fit1, dpar = "hu", probs = c(0.2, 0.8))
y <- marginal_effects(fit1, probs = c(0.2, 0.8))



## Second model

bf1 <- bf(delta_new_z ~ negsev_z + stiffness_z + (1 | ID | id))
bf2 <- bf(stiffness_z ~ negsev_z + (1 | ID | id))

prior2 <- c(
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("normal(0,2.5)", class = "b")
)

fullmod <- bf1 + bf2 + set_rescor(FALSE)
fmod <- brm(
  fullmod,
  data = sem,
  control = list(adapt_delta = 0.99),
  iter = 3000,
  prior = prior2
)

pp <- pp_check(fmod, resp = "stiffnessz", nsamples = 100)

## Tables used in manuscript

terms <- c(
  "Intercept",
  "Average NLE score",
  "Stiffness",
  "$\\Delta C$ - Football",
  "$\\Delta C$ - Rugby",
  "Intercept",
  "Average NLE score",
  "Stiffness",
  "$\\Delta C$ - Football",
  "$\\Delta C$ - Rugby"
)

datatab <- broom::tidy(fit1, par_type = "non-varying", prob = 0.95) %>%
  dendroTools::round_df(2) %>%
  slice(c(2, 7:10)) %>%
  rbind(
    .,
    broom::tidy(fit1, par_type = "non-varying") %>%
      dendroTools::round_df(2) %>%
      slice(c(1, 3:6))
  ) %>%
  mutate(term = terms) %>%
  mutate_at(vars(lower, upper), list(~ format(round(., 2), nsmall = 2))) %>%
  mutate(
    lower = str_c("[", lower, sep = ""),
    upper = str_c(upper, "]", sep = "")
  ) %>%
  unite(., "89% CR", c("lower", "upper"), sep = ", ")

terms2 <- c(
  "Intercept",
  "Average NLE score",
  "Stiffness",
  "Intercept",
  "Average NLE score"
)

datatab2 <- broom::tidy(fmod, par_type = "non-varying", prob = 0.95) %>%
  dendroTools::round_df(2) %>%
  slice(c(1, 3, 4)) %>%
  rbind(
    .,
    broom::tidy(fmod, par_type = "non-varying") %>%
      dendroTools::round_df(2) %>%
      slice(c(2, 5))
  ) %>%
  mutate(term = terms2) %>%
  mutate_at(vars(lower, upper), list(~ format(round(., 2), nsmall = 2))) %>%
  mutate(
    lower = str_c("[", lower, sep = ""),
    upper = str_c(upper, "]", sep = "")
  ) %>%
  unite(., "95% CI", c("lower", "upper"), sep = ", ")


# Save as .rds file to speed up knitting

save(graphmaker, fmod, x, y, z, pp, fit1, sem, set1, data, datatab, datatab2, file = "datavars/study2_results.rds")
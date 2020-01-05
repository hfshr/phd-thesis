## comp reliability
library(readxl)

d <- read_excel("~/Downloads/ProjectR/phdthesis/data/lesca/lescadc1reliability.xlsx", col_names = T)

library(lavaan)

map_df(d, ~sum(is.na(.))) %>% 
  gather(q, val) %>% 
  filter(val == 351) 
  
dnew <- d %>% 
  select(-c(Q6:Q8, Q87))

items <- paste(names(dnew), collapse = "+")
model <- paste("lifestress", items, sep = "=~")

dnew <- dnew[rowSums(is.na(dnew)) !=ncol(dnew), ]
dnew[is.na(dnew)] <- 0

fit <- cfa(model, data = dnew)

sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(dnew)
re <- 1 - sl^2
res <- sum(sl)^2 / (sum(sl)^2 + sum(re))

save(res, file = "datavars/lescareliability.rds")

reliability(fit)


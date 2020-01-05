load("datavars/tempnetworks.rds") # variables from study1analysischanges.R
load("datavars/networkchange.rds") # variables from study1analysisclean.R
load("datavars/modeleval.rds") # from model eval section in study1analysisclean.R
library(tidyverse)
library(sjmisc)
library(bnlearn)
library(Rgraphviz)
library(graph)

toptail <- function(data, n){
  data %>% 
    top_n(n, prob) %>% 
    rbind(., data %>% 
            top_n(n, -prob))
} # function top and bottom rows

# function for programtically performing CPQs on a given set of vars.
# can combine with map function to loop over a set of vars instead combining them.
newprobtable <- function(tallc, vars, outcome, state, model) {
  all.levels = if(any(length(vars) > 1)) { 
    lapply(tallc[, (names(tallc) %in% vars)], levels) 
  } else {
    all.levels <- tallc %>% 
      select(vars) %>% 
      sapply(levels) %>% 
      as_tibble() 
  } # makes the code work for when only one variable is used as evidence
  combos <- do.call("expand.grid", c(all.levels, list(stringsAsFactors = FALSE)))  # al combiations
  
  str1 <- "" 
  for (i in seq(nrow(combos))) {
    str1[i] = paste(combos %>% names(), " = '",
                    combos[i, ] %>% sapply(as.character), "'",
                    sep = "", collapse = ", ")
  } # generate character strings for all combinations
  str1 <- rep(str1, times = length(outcome)) # repeat the string for more than one outcome
  str1 <- paste("list(", str1, ")", sep = "")
  
  all.levels.outcome = if(any(length(outcome) > 1)) {
    lapply(tallc[, (names(tallc) %in% outcome)], levels)
  } else {
    all.levels <- tallc %>% 
      select(outcome) %>% 
      sapply(levels) %>% 
      as_tibble()
  } # repeat loop for outcome variables (can have more than one outcome)
  
  combos.outcome <- do.call("expand.grid", c(all.levels.outcome, list(stringsAsFactors = FALSE)))
  
  str3 = rep(paste("(", outcome, " == '", state, "')", sep = ""), each = length(str1)/length(outcome))  # repeat each outcome for the length of combos
  
  fitted <-  bn.fit(avg30, tallc, method = "bayes", iss = 1) # fit the model with bayes method
  cmd = paste("cpquery(fitted, ", str3, ", ", str1, ", method = 'lw', n = 500000)", sep = "") # join all elements of string together
  prob <-  rep(0, length(str1)) # empty vector for probabilities 
  for (i in seq(length(cmd))){
    prob[i] <- eval(parse(text = cmd[i]))
  } # for each combination of strings, what is the probability of outcome
  test <- cbind(combos, prob) %>% 
    mutate(outcome = str3)
  return(test)
  # output
  # predict <- lm(prob ~ ., data = combos)
  # return_list <- list("combos" = combos, "result" = test, "model" = predict,
  #                     "cmd" = cmd)
  # return(return_list)
}  

set.seed(123)

mb(avg30, "injured_1")
var <- c("balance_1", "stiffness_1", "nlelg_1", "hours")
query1 <- map_dfr(var, ~newprobtable(tallc,
                                     .x,
                                     "injured_1",
                                     "injured",
                                     avg30)) %>% 
  gather(variable, state, -prob, - outcome) %>% 
  drop_na() %>% 
  select(variable, state, prob) %>% 
  spread(state, prob) %>% 
  select(variable, Low, High) %>% 
  dendroTools::round_df(2)


query2 <- newprobtable(tallc, 
                       c("balance_1", "stiffness_1", "nlelg_1", "hours", "clevel"),
                       "injured_1",
                       "injured",
                       avg30
) %>% 
  toptail(n=3) %>% 
  arrange(-prob) %>% 
  select(outcome, everything()) %>% 
  dendroTools::round_df(2)

query2a <- query2[c(1,3),] %>% 
  dendroTools::round_df(2)


var2 <- c("balance_2", "stiffness_2", "nlelg_2", "FFFS_1", "rmssd_2")
query3 <- map_dfr(var2, ~newprobtable(tallc,
                                      .x,
                                      "injured_2",
                                      "injured",
                                      avg30)) %>% 
  gather(variable, state, -prob, - outcome) %>% 
  drop_na() %>% 
  select(variable, state, prob) %>% 
  spread(state, prob) %>% 
  select(variable, Low, High) %>% 
  dendroTools::round_df(2)


mb(avg30, "injured_2")
query4 <- newprobtable(tallc, 
                       c("balance_2", "stiffness_2", "nlelg_2", "FFFS_1", "rmssd_2"),
                       "injured_2",
                       "injured",
                       avg30
) %>% 
  toptail(3) %>% 
  arrange(-prob) %>% 
  select(outcome, everything()) %>% 
  dendroTools::round_df(2)


test <- expand.grid(list(c(-1:1),
                         c(-1:1)))
query6 <- map2_dfr(.x = as.numeric(test$Var1),
                   .y = as.numeric(test$Var2), ~tibble(prob= cpquery(fittedcont, 
                                                                     event = (injured == "injured"),
                                                                     evidence = list(nlec = .x,  stiffness = .y), method = "lw"),
                                                       nle = .x,
                                                       stiffness = .y)) 


testt <- expand.grid(list(c(-1:1),
                          c(-1:1),
                          c("Low", "High"),
                          c("injury", "no injury")))

query7 <- pmap_dfr(.l = testt, 
                   ~tibble(prob= cpquery(fittedcont, 
                                         event = (injured == "injured"),
                                         evidence = list(nlec = as.numeric(..1),  
                                                         stiffness = as.numeric(..2),
                                                         hours = ..3,
                                                         pi = ..4), method = "lw"),
                           nle = ..1,
                           stiffness = ..2,
                           hours = ..3,
                           pi = ..4)) %>% 
  toptail(3) %>% 
  arrange(-prob)



#x <- newprobtable(tallc, c("clevel", "balance_1"), "injured_1", "injured", avg30)


library(tidyverse)
library(compareGroups)
library(cobalt)

# Data -----

data <- readRDS("data/full_sample.rds") %>% droplevels()


# Covars -----

vars_for_ps <- c("fluvoxamine", 
                 "age", 
                 "gender", 
                 ## symptoms
                 "any_symptom", 
                 "fever", 
                 "cough", 
                 "sb", 
                 "ma", 
                 "confu", 
                 "headache", 
                 "sthroat", 
                 "rhinorrhoea", 
                 "cp", 
                 "diarrhoea", 
                 "nv", 
                 ## vital
                 "temp", 
                 "rr", 
                 "pr", 
                 "bp_cat130", 
                 ## comorbs
                 "any_comorb",
                 "tb", 
                 "cvd", 
                 "asthma", 
                 "copd", 
                 "diabets", 
                 "cancer", 
                 "hiv", 
                 ## clinical
                 "oxygen_level", 
                 "vaccinated_bin", 
                 "vaccinated", 
                 "dexamethasone", 
                 "inhaled_budesonide", 
                 "antibiotics", 
                 "diagnosis"
)

#saveRDS(vars_for_ps, "data/vars_for_ps.rds")

# Table - N(%) / Mean (SD) -------
tdata <- data %>% 
      mutate_if(is.logical, ~factor(as.character(.), levels = c("TRUE", "FALSE"))) %>% 
      select(all_of(vars_for_ps))
tres <- compareGroups(fluvoxamine ~ ., data = tdata, include.miss = TRUE)
tb <- createTable(tres, show.all = TRUE); tb



# SMD full ---- 
sdat <- data %>% select(all_of(vars_for_ps))
bal.tab(sdat, 
        treat = sdat$fluvoxamine, 
        stats = "mean.diffs", 
        continuous = "std", 
        binary = "std", 
        data = sdat, 
        abs = TRUE, 
        thresholds = 0.1)

# IPW ----

pdata <- data %>% select(all_of(vars_for_ps))
ps <- glm(fluvoxamine ~ ., data = pdata, family = "binomial")

ps_dat <- pdata %>% 
      mutate(psvalue = predict(object = ps, type = "response")) %>%
      mutate(weights_treat = ifelse(test = fluvoxamine, 
                                    yes = psvalue, 
                                    no = (1-psvalue))) %>%
      mutate(weights_treat_inv = ifelse(test = fluvoxamine, 
                                        yes = 1/psvalue, 
                                        no = 1/(1-psvalue))) 

#saveRDS(ps_dat, "revision code/data/ps_data_full.rds")

# SMD IPW ----
ps_balance <- bal.tab(ps_dat %>% 
                            select(-id, -starts_with("weights"), 
                                   -psvalue, -death_status, 
                                   -hospital_stay, -sympt_resolution, 
                                   -cured_symptoms), 
                      treat = ps_dat$fluvoxamine, 
                      stats = "mean.diffs",
                      weights = ps_dat$weights_treat_inv,
                      continuous = "std", 
                      binary = "std", 
                      data = sdat, 
                      abs = TRUE, 
                      thresholds = 0.1); ps_balance

ps_balance$Balance %>% 
      as.data.frame() %>% 
      filter(`M.Threshold`=="Not Balanced, >0.1") %>% 
      row.names()














































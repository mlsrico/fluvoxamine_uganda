
library(tidyverse)
library(survival)
library(coxphw)
library(cobalt)
source("utilities/CleanCox.R")


# Data ----


dat <- readRDS("data/full_sample.rds")

covars <- c("age", 
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
            "vaccinated", 
            "vaccinated", 
            "dexamethasone", 
            "inhaled_budesonide", 
            "antibiotics", 
            "diagnosis"
)

# Outcomes ---- 
tdat <- dat %>% 
      rename(status_var = death_status, 
             wait_var = hospital_stay) %>% 
      select(status_var, wait_var, fluvoxamine, all_of(covars))

covars_to_exclude <- c("any.comorb", "any.symptom")

## Sample ---- 

strat_dat <- tdat %>% 
      # Age
      #filter(age<=60) %>% 
      #filter(age>60) %>%
      #filter(gender=="Male") %>% 
      #filter(gender=="Female") %>% 
      #filter(any_symptom) %>% 
      #filter(!any_symptom) %>% 
      #filter(temp<=37.2) %>% 
      #filter(temp>37.2) %>% 
      #filter(rr<=40) %>% 
      #filter(rr>40) %>% 
      #filter(pr<=92) %>% 
      #filter(pr>92) %>% 
      #filter(bp_cat130!=">130/90") %>% 
      #filter(bp_cat130==">130/90") %>% 
      #filter(any_comorb) %>% 
      #filter(!any_comorb) %>% 
      #filter(tb) %>% 
      #filter(!tb) %>% 
      #filter(cvd) %>% 
      #filter(!cvd) %>% 
      #filter(asthma) %>% 
      #filter(!asthma) %>% 
      #filter(!copd) %>% 
      #filter(diabets) %>% 
      #filter(!diabets) %>% 
      #filter(!cancer) %>% 
      #filter(hiv) %>% 
      #filter(!hiv) %>% 
      #filter(oxygen_level=="NoO2") %>% 
      #filter(oxygen_level!="NoO2") %>% 
      #filter(oxygen_level=="<10") %>% 
      #filter(oxygen_level=="10+") %>% 
      #filter(vaccinated=="Yes,1st") %>% 
      #filter(vaccinated=="Yes,1&2") %>% 
      #filter(vaccinated!="No") %>% 
      #filter(vaccinated=="No") %>% 
      #filter(!dexamethasone) %>%
      #filter(!inhaled_budesonide) %>% 
      #filter(!antibiotics) %>% 
      #filter(diagnosis=="PCR") %>%
      #filter(diagnosis=="RDT") %>%
      #select(-diagnosis) %>% 
      droplevels()

colnames(strat_dat)[3:length(colnames(strat_dat))] <- 
      str_replace_all(colnames(strat_dat)[3:length(colnames(strat_dat))], "_", ".")

# FREQS -----
n_ctrl <- table(strat_dat$fluvoxamine)[1]
n_event_ctrl <- table(strat_dat$fluvoxamine, strat_dat$status_var)[3]
p_ctrl <- round((n_event_ctrl/n_ctrl)*100, 1)

n_exp <- table(strat_dat$fluvoxamine)[2]
n_event_exp <- table(strat_dat$fluvoxamine, strat_dat$status_var)[4]
p_exp <- round((n_event_exp/n_exp)*100, 1)

paste("Fluvoxamine: ", n_event_exp, " / ", n_exp, " (", p_exp, ")", sep = "")
paste("Control: ", n_event_ctrl, " / ", n_ctrl, " (", p_ctrl, ")", sep = "")


# Crude ----

crude <- coxph(Surv(wait_var, status_var) ~ fluvoxamine, data = strat_dat) 
#cox.zph(crude)
CleanCox(crude)


# Multi ----

multi <- coxph(Surv(wait_var, status_var) ~ ., 
               data = strat_dat %>% select(-all_of(covars_to_exclude))) 
#cox.zph(multi)
CleanCox(multi) %>% as_tibble


# Unbalanced vars ----
balance_tab <- bal.tab(strat_dat, treat = strat_dat$fluvoxamine, thresholds = 0.1)

unbalanced_vars <- balance_tab$Balance %>% 
      rownames_to_column() %>% 
      filter(!(rowname %in% c("status_var", "wait_var"))) %>% 
      filter(`M.Threshold.Un`=="Not Balanced, >0.1") %>% 
      pull(rowname) %>% 
      str_split_fixed(., "_", n=2) %>% 
      as.data.frame %>% pull(V1); unbalanced_vars

# IPW ---- 

ps <- glm(fluvoxamine ~ ., 
          data = strat_dat %>% 
                select(-wait_var, -status_var, -all_of(covars_to_exclude)), 
          family = "binomial")

ps_dat <- strat_dat %>% 
      mutate(psvalue = predict(object = ps, type = "response")) %>%
      mutate(weights_treat = ifelse(test = fluvoxamine, 
                                    yes = psvalue, 
                                    no = (1-psvalue))) %>%
      mutate(weights_treat_inv = ifelse(test = fluvoxamine, 
                                        yes = 1/psvalue, 
                                        no = 1/(1-psvalue))) 

balance_tabw <- bal.tab(ps_dat, treat = ps_dat$fluvoxamine, 
                        thresholds = 0.1, weights = ps_dat$weights_treat_inv)

unbalanced_varsw <- balance_tabw$Balance %>% 
      rownames_to_column() %>% 
      filter(!(rowname %in% c("status_var", "wait_var", 
                              "psvalue", "any.comorb", "any.symptom"))) %>% 
      filter(!str_detect(rowname, "weights")) %>% 
      filter(`M.Threshold`=="Not Balanced, >0.1") %>% 
      pull(rowname) %>% 
      str_split_fixed(., "_", n=2) %>% 
      as.data.frame %>% pull(V1); unbalanced_varsw


# IPW - crude ---- 

ipw <- coxph(Surv(wait_var, status_var) ~ fluvoxamine,
             weights = weights_treat_inv,
             data = ps_dat) 
cox.zph(ipw)
CleanCox(ipw)



# IPW - adjusted ---- 

aipw <- coxph(Surv(wait_var, status_var) ~ .,
             weights = ps_dat$weights_treat_inv,
             data = ps_dat %>% select(wait_var, status_var, 
                                      fluvoxamine,
                                      all_of(unbalanced_varsw), 
                                      #-any.comorb, 
                                      -any.symptom)) 
cox.zph(aipw)
CleanCox(aipw)


#####





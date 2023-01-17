
library(tidyverse)
library(survival)
library(survminer)
library(cowplot)
library(ggthemes)
library(coxphw)
source("revision code/utilities/CleanLog.R")


# Data ----

dat0 <- readRDS("revision code/data/ps_data_full.rds") %>% 
      select(-sympt_resolution, -cured_symptoms)
outcomes <- readRDS("data/filtered_sample_outcomes.rds") %>% 
      select(id, sympt_resolution, cured_symptoms) 

dat <- left_join(dat0, outcomes, by = "id") %>% 
      mutate(symp_status = ifelse(death_status, FALSE, cured_symptoms)) 


covars <- c("age", 
            "gender", 
            ## symptoms
            #"any_symptom", 
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
            #"vaccinated", 
            "dexamethasone", 
            "inhaled_budesonide", 
            "antibiotics", 
            "diagnosis"
)


#
# MORTALITY - freqs ----

table(dat$fluvoxamine)
table(dat$symp_status, dat$fluvoxamine)


# MORTALITY - crude ----

crude <- glm(symp_status ~ fluvoxamine, data = dat, family = "binomial") 
summary(crude)
exp(confint(crude))
CleanLog(crude)

# MORTALITY - multi ----

multi <- glm(symp_status  ~ ., 
             data = dat %>% select(symp_status, fluvoxamine, #hospital_stay, 
                                   all_of(covars), 
                                   -rr, -antibiotics, -oxygen_level, 
                                   ), 
             family = "binomial") 

CleanLog(multi)
exp(confint(multi))
car::vif(multi) %>% summary



# MORTALITY - outliers ----

out <- boxplot.stats(multi$residuals)$out
dat_out <- dat %>% mutate(residuals = multi$residuals, 
                          outliers = residuals %in% out) %>% 
      filter(!outliers)

aipw_out <- glm(symp_status  ~ ., 
                data = dat_out %>% select(symp_status, fluvoxamine, hospital_stay,
                                      all_of(covars), -rr, -antibiotics, -oxygen_level), 
                family = "binomial") 
CleanLog(aipw_out)


library(tidyverse)
library(survival)
library(survminer)
library(cowplot)
library(ggthemes)
library(coxphw)


# Data ----

dat0 <- readRDS("revision code/data/ps_data_full.rds") %>% 
      select(-sympt_resolution, -cured_symptoms)
outcomes <- readRDS("data/filtered_sample_outcomes.rds") %>% 
      select(id, sympt_resolution, cured_symptoms) 

dat <- left_join(dat0, outcomes, by = "id") %>% 
      mutate(hosp_disch = !death_status) %>% 
      filter(!death_status)


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
            #"any_comorb",
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
table(dat$hosp_disch, dat$fluvoxamine)


# MORTALITY - crude ----

crude <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine, data = dat) 
cox.zph(crude)
summary(crude)


# MORTALITY - multi ----

multi <- coxph(Surv(hospital_stay, hosp_disch) ~ ., 
               data = dat %>% select(hospital_stay, hosp_disch, fluvoxamine, 
                                     all_of(covars), -rr, -antibiotics)) 

cox.zph(multi)
summary(multi)


glmres <- glm(hosp_disch ~ .,
              data = dat %>% 
                    select(hospital_stay, hosp_disch, fluvoxamine, 
                           all_of(covars), -rr, -antibiotics), family = "binomial")
car::vif(glmres)


# MORTALITY - ipw ----

ipw <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine,
             weights = (weights_treat_inv),
             data = dat) 
cox.zph(ipw)
summary(ipw)

# MORTALITY - adj ipw ----


aipw <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine
              +ma+confu+sthroat+pr+tb+copd+cancer+hiv,
              weights = (weights_treat_inv),
              data = dat) 
cox.zph(aipw)
summary(aipw)


# MORTALITY - outliers ----

out <- boxplot.stats(aipw$residuals)$out
dat_out <- dat %>% mutate(residuals = aipw$residuals, 
                          outliers = residuals %in% out) %>% 
      filter(!outliers)

aipw_out <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine
                  +ma+confu+sthroat+pr+tb+copd+cancer+hiv,
                  weights = (weights_treat_inv),
                  data = dat_out) 
cox.zph(aipw_out)
summary(aipw_out)


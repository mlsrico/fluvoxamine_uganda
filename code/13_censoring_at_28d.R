
library(tidyverse)
library(survival)
library(survminer)
library(cowplot)
library(survey)
library(coxphw)

# Data ----

outcomes <- readRDS("data/filtered_sample_outcomes.rds") %>% 
  select(sympt_resolution, cured_symptoms) 


data0 <- readRDS("data/filtered_sample.rds") %>% 
  mutate(hosp_disch = !death_status) %>%
  droplevels()

data0$sympt_resolution <- outcomes$sympt_resolution
data0$cured_symptoms <- outcomes$cured_symptoms

data1 <- data0 %>%
  mutate(cured_symptoms = ifelse(cured_symptoms&death_status, F, cured_symptoms)) %>%
  mutate(sympt_resolution = ifelse(death_status, hospital_stay, sympt_resolution)) %>% 
  mutate(sympt_resolution = ifelse(!cured_symptoms, hospital_stay, sympt_resolution)) %>% 
  droplevels()

vars_for_ps <- readRDS("data/vars_for_ps.rds") 
vars_for_ps <- vars_for_ps[-which(vars_for_ps=="vaccinated_bin")]

# Censored status ------

data <- data1 %>% 
  ## mortality
  mutate(death_status15 = ifelse(death_status & hospital_stay>=15, FALSE, death_status)) %>% 
  mutate(death_status28 = ifelse(death_status & hospital_stay>=28, FALSE, death_status)) %>% 
  ## symptom
  mutate(cured_symptoms15 = ifelse(cured_symptoms & sympt_resolution>=15, FALSE, cured_symptoms)) %>% 
  mutate(cured_symptoms28 = ifelse(cured_symptoms & sympt_resolution>=28, FALSE, cured_symptoms)) %>%
  ## hosp
  mutate(death_status15 = ifelse(death_status & hospital_stay>=15, FALSE, death_status)) %>% 
  mutate(death_status28 = ifelse(death_status & hospital_stay>=28, FALSE, death_status))

# PS -----
ps <- glm(fluvoxamine ~ ., data = data %>% select(all_of(vars_for_ps)), family = "binomial")
ps_dat <- data %>% 
  mutate(psvalue = predict(object = ps, type = "response")) %>%
  mutate(weights_treat = ifelse(test = fluvoxamine, 
                                yes = psvalue, 
                                no = (1-psvalue))) %>%
  mutate(weights_treat_inv = ifelse(test = fluvoxamine, 
                                    yes = 1/psvalue, 
                                    no = 1/(1-psvalue))) %>% 
  droplevels()

### balance
#ps_balance <- svydesign(ids = ~ 1, data = ps_dat, weights = ~ weights_treat_inv)
#psres_balance <- svyCreateTableOne(vars = vars_for_ps[-1], 
#                                   strata = "fluvoxamine", 
#                                   data = ps_balance, 
#                                   test = TRUE)
#print(psres_balance, smd = TRUE)

# MORTALITY 15 days -----
table(data$fluvoxamine)
table(data$fluvoxamine, data$death_status15)

## crude
res <- coxph(Surv(hospital_stay, death_status15) ~ fluvoxamine,
             data = data) 

summary(res)
cox.zph(res)

## multi
res <- coxph(Surv(hospital_stay, death_status15) ~ fluvoxamine + 
                    age + gender + fever + cough + sb + ma + confu + headache + 
                    sthroat + rhinorrhoea + cp + diarrhoea + nv + temp + #rr + 
                    pr + bp_cat130 + tb + cvd + asthma + copd + diabets + 
                    cancer + hiv + oxygen_level + vaccinated + dexamethasone + 
                    inhaled_budesonide + #antibiotics + 
                   diagnosis,
              data = data) 

summary(res)
cox.zph(res)

vifres <- glm(death_status15 ~ fluvoxamine + 
      age + gender + fever + cough + sb + ma + confu + headache + 
      sthroat + rhinorrhoea + cp + diarrhoea + nv + temp + #rr + 
      pr + bp_cat130 + tb + cvd + asthma + copd + diabets + 
      cancer + hiv + oxygen_level + vaccinated + dexamethasone + 
      inhaled_budesonide + #antibiotics +
      diagnosis,
      data = data, family = "binomial" )
car::vif(vifres) %>% as.data.frame %>% map(summary)

## ipw
res <- coxph(Surv(hospital_stay, death_status15) ~ fluvoxamine,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)


## ipw adjusted
res <- coxph(Surv(hospital_stay, death_status15) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)

## outliers
out <- boxplot.stats(res$residuals)$out
ps_dat_out <- ps_dat %>% mutate(residuals = res$residuals, 
                                outliers = residuals %in% out) %>% 
  filter(!outliers)



## ipw adjusted
res <- coxph(Surv(hospital_stay, death_status15) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat_out) 

summary(res)
cox.zph(res)



# MORTALITY 28 days -----
table(data$fluvoxamine)
table(data$fluvoxamine, data$death_status28)


## crude
res <- coxph(Surv(hospital_stay, death_status28) ~ fluvoxamine,
             data = data) 

summary(res)
cox.zph(res)

## multi
res <- coxph(Surv(hospital_stay, death_status28) ~ fluvoxamine + 
                   age + gender + fever + cough + sb + ma + confu + headache + 
                   sthroat + rhinorrhoea + cp + diarrhoea + nv + temp + #rr + 
                   pr + bp_cat130 + tb + cvd + asthma + copd + diabets + 
                   cancer + hiv + oxygen_level + vaccinated + dexamethasone + 
                   inhaled_budesonide + #antibiotics + 
                   diagnosis,
             data = data) 

summary(res)
cox.zph(res)

vifres <- glm(death_status28 ~ fluvoxamine + 
                    age + gender + fever + cough + sb + ma + confu + headache + 
                    sthroat + rhinorrhoea + cp + diarrhoea + nv + temp + #rr + 
                    pr + bp_cat130 + tb + cvd + asthma + copd + diabets + 
                    cancer + hiv + oxygen_level + vaccinated + dexamethasone + 
                    inhaled_budesonide + #antibiotics +
                    diagnosis,
              data = data, family = "binomial" )
car::vif(vifres) %>% as.data.frame %>% map(summary)

## ipw
res <- coxph(Surv(hospital_stay, death_status28) ~ fluvoxamine,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)

## ipw adjust
res <- coxph(Surv(hospital_stay, death_status28) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)


## outliers

out <- boxplot.stats(res$residuals)$out
ps_dat_out <- ps_dat %>% mutate(residuals = res$residuals, 
                                outliers = residuals %in% out) %>% 
  filter(!outliers)

res <- coxph(Surv(hospital_stay, death_status28) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat_out) 

summary(res)
cox.zph(res)

# PS survivors -------
survivors <- data %>% filter(!death_status)

ps <- glm(fluvoxamine ~ ., data = survivors %>% select(all_of(vars_for_ps)), family = "binomial")
ps_dat <- survivors %>% 
  mutate(psvalue = predict(object = ps, type = "response")) %>%
  mutate(weights_treat = ifelse(test = fluvoxamine, 
                                yes = psvalue, 
                                no = (1-psvalue))) %>%
  mutate(weights_treat_inv = ifelse(test = fluvoxamine, 
                                    yes = 1/psvalue, 
                                    no = 1/(1-psvalue))) %>% 
  droplevels()

# SYMPTOM 15 days -----
table(survivors$fluvoxamine)
table(survivors$fluvoxamine, survivors$cured_symptoms15)

## crude
res <- coxph(Surv(sympt_resolution, cured_symptoms15) ~ fluvoxamine,
             data = survivors) 

summary(res)
cox.zph(res)

## ipw
res <- coxph(Surv(sympt_resolution, cured_symptoms15) ~ fluvoxamine,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)

## ipw adj
res <- coxph(Surv(sympt_resolution, cured_symptoms15) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat) 

summary(res)
cox.zph(res)


## outliers
out <- boxplot.stats(res$residuals)$out
ps_dat_out <- ps_dat %>% mutate(residuals = res$residuals, 
                                outliers = residuals %in% out) %>% 
  filter(!outliers)

res <- coxph(Surv(sympt_resolution, cured_symptoms15) ~ fluvoxamine
             +temp+rr+tb+cancer+antibiotics,
             weights = weights_treat_inv,
             data = ps_dat_out) 

summary(res)
cox.zph(res)


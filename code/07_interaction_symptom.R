
library(tidyverse)
library(survival)
library(coxphw)
library(cobalt)
source("revision code/utilities/CleanLog.R")


# Data ----

#dat0 <- readRDS("revision code/data/ps_data_full.rds") %>% 
#      select(-sympt_resolution, -cured_symptoms)
#outcomes <- readRDS("data/filtered_sample_outcomes.rds") %>% 
#      select(id, sympt_resolution, cured_symptoms) 

#dat <- left_join(dat0, outcomes, by = "id")

dat <- readRDS("data/data_for_all_outcomes.rds")

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
      mutate(symp_status = ifelse(death_status, FALSE, cured_symptoms)) %>%
      rename(status_var = symp_status) %>% 
      select(status_var, fluvoxamine, all_of(covars))

covars_to_exclude <- c("any.comorb", "any.symptom"
                       #"antibiotics", "rr"
                       )

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
      filter(vaccinated=="No") %>% 
      #filter(!dexamethasone) %>%
      #filter(!inhaled_budesonide) %>% 
      #filter(antibiotics) %>% 
      #filter(diagnosis=="PCR") %>%
      #filter(diagnosis=="RDT") %>%
      
      select(-vaccinated) %>% 
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

crude <- glm(status_var ~ fluvoxamine, data = strat_dat, family = "binomial") 
CleanLog(crude)


# Multi ----

multi <- glm(status_var ~ ., 
               data = strat_dat %>% select(-all_of(covars_to_exclude),
                                           ), 
             family = "binomial") 

multicol_vars <- car::vif(multi) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(GVIF>2.5) %>% 
      pull(rowname) %>% cat(., sep = ", "); multicol_vars


CleanLog(multi)



tableone::CreateTableOne(vars = strat_dat %>% select(-status_var, -all_of(covars_to_exclude)) %>% colnames, 
                         strata = "status_var", 
                         data = strat_dat %>% select(-all_of(covars_to_exclude)))




# Interaction ----- 

inter_dat <- tdat %>% 
      mutate(age = age>60) %>% 
      mutate(temp = temp>37.2) %>% 
      mutate(rr = rr>40) %>% 
      mutate(pr = pr>92) %>% 
      #mutate(oxygen_level = oxygen_level!="NoO2") %>% 
      #mutate(vaccinated = vaccinated!="No") %>% 
      droplevels()


inter <- glm(status_var ~ fluvoxamine*antibiotics
             + age
             + gender 
             + fever 
             + cough 
             + sb 
             + ma 
             + confu 
             + headache 
             + sthroat 
             + rhinorrhoea 
             + cp 
             + diarrhoea 
             + nv 
             + temp 
             + rr
             + pr 
             + bp_cat130 
             + tb 
             + cvd 
             + asthma 
             + copd 
             + diabets 
             + cancer 
             + hiv 
             + oxygen_level 
             + vaccinated 
             + dexamethasone 
             + inhaled_budesonide 
             #+ antibiotics
             + diagnosis, 
             data = inter_dat, 
             family = "binomial")

CleanLog(inter) %>% as.data.frame









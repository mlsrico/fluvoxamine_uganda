#test
library(tidyverse)
library(readxl)
library(mice)

# Original data --------
or_data <- read_excel("data/fluvoxamine_data.xlsx") %>% 
      mutate(id = 1:316)

saveRDS(or_data, "data/full_data_id.rds")


# Mutate vars ------

fix_data <- or_data %>% 

      # format
      mutate_at(vars(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, cp, diarrhoea, nv), ~.=="TRUE") %>% # symptoms
      mutate(tempcat = factor(tempcat, levels = c("<37.5", "37.5-38.0", "38.1-39.0", ">39.0"))) %>%
      mutate_at(vars(tb, cvd, asthma, copd, diabets, cancer, hiv), ~.=="TRUE") %>% # comorbs
      mutate_at(vars(starts_with("bp_cat")), as.factor) %>% ## blood preassure
      mutate_at(vars(starts_with("medic")), ~.=="TRUE") %>% # oxygen and meds
      mutate_at(vars(starts_with("fvx_effect")), ~.=="TRUE") %>% ## adverse effects
      mutate(vaccinated = as.factor(vaccinated)) %>%


      # change values
      mutate(death_status = death_status=="TRUE") %>% 
library(readxl)
      mutate(oxygen_level = factor(oxygen_level, levels = c("NoO2", "<10", "10+"))) %>% 
      mutate(oxygen_need = oxygen_level!="NoO2") %>% 
      #mutate(diagnosis = factor(as.character(diagnosis), 
      #                          levels = c("PCR positive", "RDT", "Radiology CXR/CT"), 
      #                          labels = c("PCR", "RDT", "CXR/CT"))) 
      


# New variables ----

sel_data <- sel_data0 %>% 
      mutate(any_symptom2 = rowSums(sel_data0 %>% select(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, cp, diarrhoea, nv))>0) %>% ## any symptom
      mutate(any_comorb = rowSums(sel_data0 %>% select(tb, cvd, asthma, copd, diabets, cancer, hiv))>0) %>% ## any comorb
      mutate(any_adverse_effect = rowSums(sel_data0 %>% select(starts_with("fvx_effect")))>0)



# Dealing with missing data -------

miss_data0 <- sel_data %>% 
      mutate(bp_cat130 = as.factor(ifelse(is.na(bp_cat130), "?130/90", as.character(bp_cat130)))) %>% ## merge missing with normal bp
      mutate(temp = ifelse(temp==27.6, 37.6, temp)) %>% ## fix temp
      #mutate(temp = ifelse(is.na(temp & !fever), 37, temp)) %>% ## patients with missing temp but no fever, 37ºc
      #mutate(rr = ifelse(rr<10, 10, rr)) %>% ## fix rr
      mutate(death_status = ifelse(is.na(death_status), FALSE, death_status)) ## fix death_status


vars_needed <- c("id", "fluvoxamine", 
                 "death_status", "cured_symptoms",
                 "hospital_stay", "sympt_resolution",
                 "age", "gender", 
                 ## symptoms
                 "any_symptom", 
                 "fever", "cough", "sb", "ma", "confu", "headache", "sthroat", 
                 "rhinorrhoea", "cp", "diarrhoea", "nv", 
                 ## vital
                 "temp", "rr", "pr", "bp_cat130", 
                 ## comorbs
                 "any_comorb",
                 "tb", "cvd", "asthma", "copd", "diabets", "cancer", "hiv", 
                 ## clinical
                 "oxygen_level", 
                 "vaccinated", 
                 "dexamethasone", 
                 "inhaled_budesonide", 
                 "antibiotics", 
                 "diagnosis")

miss_data00 <- miss_data0 %>% select(all_of(vars_needed))
imp_set <- mice(miss_data00, m=5, maxit = 50, method = 'pmm', seed = 500)
imp_data <- complete(imp_set,4)



# Filter ------ 

filt_data <- imp_data %>% filter(diagnosis != "CXR/CT")


# Save ------

#saveRDS(imp_data, "data/full_sample.rds")
#saveRDS(filt_data, "data/filtered_sample.rds")


#saveRDS(imp_data, "data/full_sample_outcomes.rds")
#saveRDS(filt_data, "data/filtered_sample_outcomes.rds")



 

















#test
library(tidyverse)
library(mice)

# Original data --------
or_data <- read_csv("data/Fluvoxamine_data.csv") %>% 
      mutate(id = 1:333)

#saveRDS(or_data, "data/full_data_id.rds")

# Baseline characteristics -----

sel_vars <- c(
            # basic
            "id", "age", "agegroup0", "gender", 
            "medic20", ## Fluvoxamine
            
            # status
            "patient_status", ## Patients status (2-Discharged; 3-Died)
            "death_status",
            "sympt_resolution", ## Days to symptom resolution
            "hospital_stay", ## Length of hospital stay
            "cured_symptoms",
            
            # symptoms
            "fever", 
            "cough", 
            "sb", ## Shortness of breath
            "ma", ## Muscle ache
            "confu", ## Confusion
            "headache", 
            "sthroat", ## Sore throat
            "rhinorrhoea", 
            "cp", ## Chest pain
            "diarrhoea", 
            "nv", ## Nausea and vomiting
            
            # vital signs
            "temp", ## Temperature
            "tempcat", ## Temperature (1:<37.5; 2:37.5-38.0; 3:38.1-39.0; 4:>39.0)
            "rr", ## Respiratory rate
            "pr", ## Pulse rate
            #"bp", ## Blood Pressure
            "bp_cat130", "bp_cat140", ## Blood Pressure
            
            
            # comorbidities
            "tb", ## Tuberculosis
            "cvd", ## Heart disease (CVD)
            "asthma", ## Asthma
            "copd", ## COPD
            "diabets", ## Diabetes
            "cancer", ## Cancer
            "hiv", ## HIV
            
            # oxygen
            #"medic6", ## 0
            #"medic7", ## 1-<5
            #"medic8", ## 5-10
            #"medic9", ## 10-15
            #"medic10", ## 15-20
            #"medic11", ## 20-40
            #"medic12", ## 40-60
            "oxygen_level", 
            "oxygen_need",
            
            # vaccination
            "vaccinated", ## Have you been vaccinated (1-No; 2-1st; 3-1st&2nd)
            
            # key covid meds
            "medic1", ## Dexamethasone
            "medic15", ## Inhaled budesonide 
            "antibiotics", ## Antibiotics
            
            # diagnosis
            "diagnosis", ## Diagnosis method (1-PCR; 2-RDT; 4-CXR/CT) 

            # side effects
            "fvx_effect1", ## Nausea
            "fvx_effect2", ## Vomiting
            "fvx_effect3", ## Stomach pain
            "fvx_effect4", ## Constipation
            "fvx_effect5", ## Diarrhea
            "fvx_effect6", ## Heartburn
            "fvx_effect7", ## Loss of appetite
            "fvx_effect8", ## Dry mouth
            "fvx_effect9", ## Drowsiness
            "fvx_effect10", ## Difficulty sleeping
            "fvx_effect11", ## Dizziness
            "fvx_effect12", ## Nervousness
            "fvx_effect13", ## Feeling anxious
            "fvx_effect14", ## Headache
            "fvx_effect15", ## Muscle weakness
            "fvx_effect16", ## Muscle pains
            "fvx_effect17", ## Pins and needles
            "fvx_effect18", ## Abnormal taste
            "fvx_effect19", ## Faster heartbeat
            "fvx_effect20", ## Sweating
            "fvx_effect21", ## Fainting
            "fvx_effect22", ## Black stools, vomit that looks like coffee grounds
            "fvx_effect23", ## Seizures
            "fvx_effect24", ## Eye pain/swelling/redness
            "fvx_effect25" ## Widened pupils, vision changes
            ## Back pain
            ## Cough, general body weakness
            ## Delirium
            #"fvx_effect26" ## Others
            )



# Mutate vars ------

fix_data <- or_data %>% 

      # format
      mutate(gender = factor(gender, levels = c("Male", "Female"))) %>% 
      mutate_at(vars(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, cp, diarrhoea, nv), ~.=="Yes") %>% # symptoms
      mutate(tempcat = factor(tempcat, levels = c("<37.5", "37.5-38.0", "38.1-39.0", ">39.0"))) %>%
      mutate_at(vars(tb, cvd, asthma, copd, diabets, cancer, hiv), ~.=="Yes") %>% # comorbs
      mutate_at(vars(starts_with("bp_cat")), as.factor) %>% ## blood preassure
      mutate_at(vars(starts_with("medic")), ~.=="Yes") %>% # oxygen and meds
      mutate_at(vars(starts_with("fvx_effect")), ~.=="Yes") %>% ## adverse effects
      mutate(agegroup0 = as.factor(agegroup0)) %>%
      mutate(antibiotics = as.logical(antibiotics)) %>%
      mutate(vaccinated = as.factor(vaccinated)) %>%


      # change values
      mutate(death_status = patient_status=="Died") %>% 
      #mutate(oxygen_level = ifelse(medic6, "NoO2", 
      #                             ifelse(medic7, "1-<5", 
      #                                    ifelse(medic8|medic9, "5-15", 
      #                                           ifelse(medic10|medic11|medic12, ">15", "NoO2"))))) %>%
      #mutate(oxygen_level = factor(oxygen_level, levels = c("NoO2", "1-<5", "5-15", ">15"))) %>% 
      mutate(oxygen_level = ifelse(medic6, "NoO2", 
                                   ifelse(medic7|medic8, "<10", 
                                          ifelse(medic8|medic9, "10+", "NoO2")))) %>%
      mutate(oxygen_level = factor(oxygen_level, levels = c("NoO2", "<10", "10+"))) %>% 
      mutate(oxygen_need = oxygen_level!="NoO2") %>% 
      mutate(diagnosis = factor(as.character(diagnosis), 
                                levels = c("PCR positive", "RDT", "Radiology CXR/CT"), 
                                labels = c("PCR", "RDT", "CXR/CT"))) %>% 
      mutate(cured_symptoms = !is.na(sympt_resolution)) %>% 
      mutate(cured_symptoms = ifelse(cured_symptoms&death_status, F, cured_symptoms)) %>%
      mutate(sympt_resolution = ifelse(is.na(sympt_resolution), max(sympt_resolution, na.rm=TRUE), sympt_resolution)) %>% 
      mutate(sympt_resolution = ifelse(death_status, hospital_stay, sympt_resolution)) %>% 
      mutate(sympt_resolution = ifelse(!cured_symptoms, hospital_stay, sympt_resolution))
      

# Select data ----
sel_data0 <- fix_data %>% select(all_of(sel_vars)) %>% 
      rename(fluvoxamine = medic20) %>%
      rename(dexamethasone = medic1) %>% 
      rename(inhaled_budesonide = medic15) 

# New variables ----

sel_data <- sel_data0 %>% 
      mutate(any_symptom = rowSums(sel_data0 %>% select(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, cp, diarrhoea, nv))>0) %>% ## any symptom
      mutate(any_comorb = rowSums(sel_data0 %>% select(tb, cvd, asthma, copd, diabets, cancer, hiv))>0) %>% ## any comorb
      mutate(any_adverse_effect = rowSums(sel_data0 %>% select(starts_with("fvx_effect")))>0)



# Dealing with missing data -------

miss_data0 <- sel_data %>% 
      mutate(bp_cat130 = as.factor(ifelse(is.na(bp_cat130), "?130/90", as.character(bp_cat130)))) %>% ## merge missing with normal bp
      mutate(temp = ifelse(temp==27.6, 37.6, temp)) %>% ## fix temp
      #mutate(temp = ifelse(is.na(temp & !fever), 37, temp)) %>% ## patients with missing temp but no fever, 37ºc
      mutate(rr = ifelse(rr<10, 10, rr)) %>% ## fix rr
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



 

















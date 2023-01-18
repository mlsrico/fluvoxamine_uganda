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
      mutate_at(vars(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, 
                     cp, diarrhoea, nv, starts_with("fvx_effect"), death_status,
                     starts_with("medic"), tb, cvd, asthma, copd, diabets, 
                     cancer, hiv),
                ~.=="TRUE") %>% # symptoms
      #mutate(tempcat = factor(tempcat, levels = c("<37.5", "37.5-38.0", "38.1-39.0", ">39.0"))) %>%
      mutate_at(vars(starts_with("bp_cat")), as.factor) %>% ## blood preassure
      mutate(vaccinated = as.factor(vaccinated)) %>%
      mutate(oxygen_level = factor(oxygen_level, levels = c("NoO2", "<10", "10+"))) %>% 
      mutate(oxygen_need = oxygen_level!="NoO2") %>% 
      mutate(vaccinated_bin = vaccinated != "No")
      


# New variables ----

sel_data <- fix_data %>% 
      mutate(any_symptom2 = rowSums(fix_data %>% select(fever, cough, sb, ma, confu, headache, sthroat, rhinorrhoea, cp, diarrhoea, nv))>0) %>% ## any symptom
      mutate(any_comorb = rowSums(fix_data %>% select(tb, cvd, asthma, copd, diabets, cancer, hiv))>0) %>% ## any comorb
      mutate(any_adverse_effect = rowSums(fix_data %>% select(starts_with("fvx_effect")))>0)


# Save ------

saveRDS(sel_data, "data/full_sample.rds")















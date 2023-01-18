
library(tidyverse)
library(survival)
library(nnt)
library(meta)

dat <- readRDS("data/data_for_all_outcomes.rds") 


# Survival - mortality ----
time <- dat$hospital_stay %>% as.numeric
status <- dat$death_status %>% as.numeric
arm <- dat$fluvoxamine %>% as.numeric

KM2NNT(time, status, arm, tau = NULL, confint = 0.95, digits = 3)


# Logistic - symptom resolution ----
nnt(x = 2.56, p.c = 0.3108, sm = "OR")







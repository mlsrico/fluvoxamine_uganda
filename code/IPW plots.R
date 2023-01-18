


library(tidyverse)
library(survival)
library(survminer)
library(cowplot)
library(ggthemes)
library(coxphw)


# Data ----

data <- readRDS("data/full_sample.rds") %>% 
      mutate(cured_symptoms = ifelse(cured_symptoms&death_status, F, cured_symptoms)) %>%
      mutate(sympt_resolution = ifelse(death_status, hospital_stay, sympt_resolution)) %>% 
      mutate(sympt_resolution = ifelse(!cured_symptoms, hospital_stay, sympt_resolution)) %>% 
      droplevels()

vars_for_ps <- readRDS("data/vars_for_ps.rds"); vars_for_ps <- vars_for_ps[-which(vars_for_ps=="vaccinated_bin")]
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



## full sample ----- 

full_p <- ps_dat %>% 
      mutate(weights_treat_inv = ifelse(weights_treat_inv>quantile(weights_treat_inv, probs = .99), 
                                        quantile(ps_dat$weights_treat_inv, probs = .99), 
                                        weights_treat_inv)) %>% 
      mutate(fluvoxamine = ifelse(fluvoxamine, "Fluvoxamine", "No fluvoxamine")) %>% 
      ggplot(aes(x = weights_treat_inv, y = ..density..,
                 color = fluvoxamine, fill = fluvoxamine)) +
      geom_histogram(alpha = 0.8, bins = 50) +
      scale_x_continuous(limits = c(0, 16), breaks = seq(1, 20, by = 4)) +
      scale_color_manual(values = c("#e63946", "#457b9d")) + scale_fill_manual(values = c("#e63946", "#457b9d")) +
      facet_grid(. ~ fluvoxamine, scales = "free_x") +
      labs(x = "Inverse propensity score weights", y = "Density") +
      theme_bw() +
      theme(legend.position = "none", 
            strip.text.x = element_text(size = 12),
            axis.title= element_text(size = 12), 
            axis.text = element_text(size = 12)); full_p

## alive ----
alive_p <- ps_dat %>% 
      filter(!death_status) %>% 
      mutate(weights_treat_inv = ifelse(weights_treat_inv>quantile(weights_treat_inv, probs = .99), 
                                        quantile(ps_dat$weights_treat_inv, probs = .99), 
                                        weights_treat_inv)) %>% 
      mutate(fluvoxamine = ifelse(fluvoxamine, "Fluvoxamine", "No fluvoxamine")) %>% 
      ggplot(aes(x = weights_treat_inv, y = ..density..,
                 color = fluvoxamine, fill = fluvoxamine)) +
      geom_histogram(alpha = 0.8, bins = 50) +
      scale_x_continuous(limits = c(0, 16), breaks = seq(1, 20, by = 4)) +
      scale_color_manual(values = c("#e63946", "#457b9d")) + scale_fill_manual(values = c("#e63946", "#457b9d")) +
      facet_grid(. ~ fluvoxamine, scales = "free_x") +
      labs(x = "Inverse propensity score weights", y = "Density") +
      theme_bw() +
      theme(legend.position = "none", 
            strip.text.x = element_text(size = 12),
            axis.title= element_text(size = 12), 
            axis.text = element_text(size = 12)); alive_p

## combine ----

plot_grid(full_p, alive_p, ncol = 1, labels = "AUTO")




library(tidyverse)
library(cowplot)
library(ggthemes)
library(MatchIt)
library(cobalt)


# Data ----


data <- readRDS("data/full_sample.rds") %>%
      mutate(cured_symptoms = ifelse(cured_symptoms&death_status, F, cured_symptoms)) %>%
      mutate(sympt_resolution = ifelse(death_status, hospital_stay, sympt_resolution)) %>% 
      mutate(sympt_resolution = ifelse(!cured_symptoms, hospital_stay, sympt_resolution)) %>% 
      droplevels()

#saveRDS(data, "data/data_for_all_outcomes.rds")
#write.csv(data, "data/full_data_fluvoxamine.csv", row.names = F)

vars_for_ps <- readRDS("data/vars_for_ps.rds") 
vars_for_ps <- vars_for_ps[-which(vars_for_ps=="vaccinated_bin")]


# Matchit Full ------

mdata <- data %>% select(all_of(vars_for_ps))

m.out <- matchit(fluvoxamine ~ ., 
                 data = mdata,
                 replace = FALSE)

#summary(m.out)
dmatch <- match.data(m.out) %>% 
      select(all_of(vars_for_ps))
      

bal.tab(dmatch, 
        treat = dmatch$fluvoxamine, 
        stats = "mean.diffs", 
        continuous = "std", 
        binary = "std", 
        data = dmatch, 
        abs = TRUE, 
        thresholds = 0.1)


# Plot Full -----

bplot <- bal.plot(m.out, 
                  var.name = "distance", 
                  which = "both",
                  type = "histogram", 
                  #colors = c("#e63946", "#457b9d"),
                  mirror = TRUE) +
      labs(title = NULL, fill = "", x = "Distance") +
      scale_fill_manual(values = c("#e63946", "#457b9d"), 
                        labels = c("No Fluvoxamine", "Fluvoxamine"), 
                        guide = guide_legend(reverse = TRUE)) +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            legend.position = "top", 
            legend.justification = "center", 
            strip.text.x = element_text(size = 13),
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 14), 
            legend.text = element_text(size = 13))


bplot

# Matchit Alive ------

mdata1 <- data %>% 
      filter(!death_status) %>% 
      select(all_of(vars_for_ps))

m.out1 <- matchit(fluvoxamine ~ ., 
                 data = mdata1,
                 replace = FALSE)

dmatch1 <- match.data(m.out1) %>% 
      select(all_of(vars_for_ps))


bal.tab(dmatch1, 
        treat = dmatch1$fluvoxamine, 
        stats = "mean.diffs", 
        continuous = "std", 
        binary = "std", 
        data = dmatch1, 
        abs = TRUE, 
        thresholds = 0.1)

#summary(m.out)


# Plot Alive -----

bplot1 <- bal.plot(m.out1, 
                  var.name = "distance", 
                  which = "both",
                  type = "histogram", 
                  #colors = c("#e63946", "#457b9d"),
                  mirror = TRUE) +
      labs(title = NULL, fill = "", x = "Distance") +
      scale_fill_manual(values = c("#e63946", "#457b9d"), 
                        labels = c("No Fluvoxamine", "Fluvoxamine"), 
                        guide = guide_legend(reverse = TRUE)) +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            legend.position = "top", 
            legend.justification = "center", 
            strip.text.x = element_text(size = 13),
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 14), 
            legend.text = element_text(size = 13))


bplot1

# COMBINE ---- 
plot_grid(bplot, bplot1, ncol = 1, labels = "AUTO")






library(tidyverse)
library(cowplot)
library(ggthemes)
library(MatchIt)
library(cobalt)
library(survminer)
library(survival)
library(compareGroups)
#

# Data ----

outcomes <- readRDS("data/filtered_sample_outcomes.rds") %>% 
      filter(!death_status) %>% 
      select(sympt_resolution, cured_symptoms) 


data0 <- readRDS("data/filtered_sample.rds") %>% 
      mutate(hosp_disch = !death_status) %>%
      filter(!death_status) %>% 
      droplevels()

data0$sympt_resolution <- outcomes$sympt_resolution
data0$cured_symptoms <- outcomes$cured_symptoms

data <- data0 %>%
      mutate(cured_symptoms = ifelse(cured_symptoms&death_status, F, cured_symptoms)) %>%
      mutate(sympt_resolution = ifelse(death_status, hospital_stay, sympt_resolution)) %>% 
      mutate(sympt_resolution = ifelse(!cured_symptoms, hospital_stay, sympt_resolution)) %>% 
      droplevels()

#saveRDS(data, "data/data_for_all_outcomes.rds")
#write.csv(data, "data/full_data_fluvoxamine.csv", row.names = F)

vars_for_ps <- readRDS("data/vars_for_ps.rds") 
vars_for_ps <- vars_for_ps[-which(vars_for_ps=="vaccinated_bin")]


# MORTALITY - matchit ------

m.out <- matchit(fluvoxamine ~ age + gender + fever + cough + sb +
                               ma + confu + headache + sthroat + rhinorrhoea +
                               cp + diarrhoea + nv + temp + rr + pr + bp_cat130 + 
                               tb + cvd + asthma + copd + diabets +
                               cancer + hiv + oxygen_level + vaccinated + dexamethasone +
                               inhaled_budesonide + antibiotics + diagnosis, 
                 #method = "optimal",
                 ratio = 1,
                 data = data, 
                 replace = FALSE)

mdat <- match.data(m.out)


tres <- compareGroups(fluvoxamine ~ ., 
                      data = mdat %>% 
                            select(all_of(vars_for_ps)) %>% 
                            mutate_if(is.logical, as.character), 
                      include.miss = TRUE)
tb <- createTable(tres, show.all = TRUE); tb
#kableExtra::kable(tb$descr, format="markdown")

# MORTALITY - COX ----

## crude
res <- coxph(Surv(hospital_stay, death_status) ~ fluvoxamine,
             data = mdat) 

summary(res)
cox.zph(res)


## multi
res <- coxph(Surv(hospital_stay, death_status) ~ fluvoxamine
             +fever+cough+headache+diarrhoea+rr+bp_cat130+copd+hiv+oxygen_level,
             data = mdat) 

summary(res)
cox.zph(res)



# MORTALITY - km ----

fit <- survfit(Surv(hospital_stay, death_status) ~ fluvoxamine,
               data = mdat)

mort_plot <- ggsurvplot(fit, data = mdat, #fun = "cumhaz", 
                        # axis
                        xlim = c(0, 25), 
                        #xlim = c(0, 20), 
                        break.x.by = 5, 
                        #ylim = c(0, 2.1),
                        
                        # inside plot
                        conf.int = TRUE,
                        legend.labs = c("No Fluvoxamine", "Fluvoxamine"),
                        
                        # legend
                        legend.title = "", 
                        font.legend = c(14), 
                        palette = c("#e63946", "#457b9d"),
                        
                        # table
                        risk.table = TRUE, 
                        risk.table.fontsize = 4.5, 
                        risk.table.title = "Individuals at risk", 
                        risk.table.title.size = 18, 
                        
                        # format
                        #fontsize = 30, 
                        font.x = c(16),
                        font.y = c(16),
                        font.tickslab = c(14),
                        ggtheme = theme_half_open()) 


mort_plot$plot <- mort_plot$plot +
      labs(x = "Days") +
      theme(legend.justification = "center", 
            axis.title.y = element_text(face = "bold"))


mort_plot$table <- mort_plot$table +
      labs(x = "Days") +
      theme(title = element_text(size = 13), axis.ticks.y = element_blank(),
            axis.text = element_text(size = 13),
            axis.title.x = element_text(size = 16))

mort_plot

# HOSPITAL - matchit ------

m.out <- matchit(fluvoxamine ~ age + gender + fever + cough + sb +
                       ma + confu + headache + sthroat + rhinorrhoea +
                       cp + diarrhoea + nv + temp + rr + pr + bp_cat130 + 
                       tb + cvd + asthma + copd + diabets +
                       cancer + hiv + oxygen_level + vaccinated + dexamethasone +
                       inhaled_budesonide + antibiotics + diagnosis, 
                 #method = "optimal",
                 ratio = 1,
                 data = data %>% filter(!death_status), 
                 replace = FALSE)

mdat <- match.data(m.out)

tres <- compareGroups(fluvoxamine ~ ., 
                      data = mdat %>% 
                            select(all_of(vars_for_ps)) %>% 
                            mutate_if(is.logical, as.character), 
                      include.miss = TRUE)
tb <- createTable(tres, show.all = TRUE); tb
kableExtra::kable(tb$descr, format="markdown")

# HOSPITAL - COX ----

## crude
res <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine,
             data = mdat) 

summary(res)
cox.zph(res)

## multi
res <- coxph(Surv(hospital_stay, hosp_disch) ~ fluvoxamine
             +age+fever+cough+cp+nv+temp+pr+asthma+copd+cancer+hiv+oxygen_level+dexamethasone+diagnosis, 
             data = mdat)


summary(res)
cox.zph(res)

# HOSPITAL - km ----

fit <- survfit(Surv(hospital_stay, hosp_disch) ~ fluvoxamine,
               data = mdat)

hosp_plot <- ggsurvplot(fit, data = mdat, #fun = "cumhaz", 
                        # axis
                        xlim = c(0, 25), 
                        #xlim = c(0, 20), 
                        break.x.by = 5, 
                        #ylim = c(0, 2.1),
                        
                        # inside plot
                        conf.int = TRUE,
                        legend.labs = c("No Fluvoxamine", "Fluvoxamine"),
                        
                        # legend
                        legend.title = "", 
                        font.legend = c(14), 
                        palette = c("#e63946", "#457b9d"),
                        
                        # table
                        risk.table = TRUE, 
                        risk.table.fontsize = 4.5, 
                        risk.table.title = "Individuals at risk", 
                        risk.table.title.size = 18, 
                        
                        # format
                        #fontsize = 30, 
                        font.x = c(16),
                        font.y = c(16),
                        font.tickslab = c(14),
                        ggtheme = theme_half_open()) 


hosp_plot$plot <- hosp_plot$plot +
      labs(x = "Days") +
      theme(legend.justification = "center", 
            axis.title.y = element_text(face = "bold"))


hosp_plot$table <- hosp_plot$table +
      labs(x = "Days") +
      theme(title = element_text(size = 13), axis.ticks.y = element_blank(),
            axis.text = element_text(size = 13),
            axis.title.x = element_text(size = 16))

hosp_plot


# SYMPTOM - matchit ------

m.out <- matchit(fluvoxamine ~ age + gender + fever + cough + sb +
                       ma + confu + headache + sthroat + rhinorrhoea +
                       cp + diarrhoea + nv + temp + rr + pr + bp_cat130 + 
                       tb + cvd + asthma + copd + diabets +
                       cancer + hiv + oxygen_level + vaccinated + dexamethasone +
                       inhaled_budesonide + antibiotics + diagnosis, 
                 #method = "optimal",
                 ratio = 1,
                 data = data, 
                 replace = FALSE)

mdat <- match.data(m.out) %>% 
      mutate(cured_symptoms = ifelse(death_status, FALSE, cured_symptoms))

# SYMPTOM - glm ----

## crude
res <- glm(cured_symptoms ~ fluvoxamine,
             data = mdat, family = "binomial") 

summary(res)
exp(confint(res))

## multi
res <- glm(cured_symptoms ~ fluvoxamine
           +fever+cough+headache+diarrhoea+rr+bp_cat130+copd+hiv+oxygen_level,
           data = mdat, family = "binomial") 

summary(res)
exp(confint(res))



## Combine ---- 

lg <- get_legend(mort_plot$plot)

# plots 
mort_plot$plot <- mort_plot$plot + theme(legend.position="none", axis.title.x = element_blank())
hosp_plot$plot <- hosp_plot$plot + theme(legend.position="none", axis.title.y = element_blank(), axis.title.x = element_blank())
#sym_plot$plot <- sym_plot$plot + theme(legend.position="none", axis.title.y = element_blank(), axis.title.x = element_blank())


# tables
mort_plot$table <- mort_plot$table + theme(axis.text.y.left = element_text(size=11))
hosp_plot$table <- hosp_plot$table + theme(axis.text.y.left = element_text(size=11))
#sym_plot$table <- sym_plot$table + theme(axis.text.y.left = element_text(size=11))


# combine
ggcrude <- plot_grid(mort_plot$plot, mort_plot$table, ncol = 1, align = "v", rel_heights = c(1, 0.5)); ggcrude
ggipw <- plot_grid(hosp_plot$plot, hosp_plot$table, ncol = 1, align = "v", rel_heights = c(1, 0.5)); ggipw
#ggouts <- plot_grid(sym_plot$plot, sym_plot$table, ncol = 1, align = "v", rel_heights = c(1, 0.5)); ggouts

km <- plot_grid(ggcrude, ggipw, nrow=1, labels="AUTO", label_size=18); km

km_lg <- plot_grid(lg, km, ncol=1, rel_heights = c(0.1, 1)); km_lg






























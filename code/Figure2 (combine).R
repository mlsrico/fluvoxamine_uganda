

library(tidyverse)
library(survival)
library(survminer)
library(cowplot)

# Mortality ----
crude <- readRDS("mortality_crude.rds")
ipw <- readRDS("mortality_ipw.rds")
outs <- readRDS("mortality_outliers.rds")

lg <- get_legend(crude$plot)

## plots
crude$plot <- crude$plot + theme(legend.position = "none")
ipw$plot <- ipw$plot + theme(legend.position = "none")
outs$plot <- outs$plot + theme(legend.position = "none")

mort_plots <- plot_grid(crude$plot, ipw$plot, outs$plot, nrow=1, labels = "AUTO")
mort_plots_lg <- plot_grid(lg, mort_plots, ncol=1, rel_heights = c(0.1, 1)); mort_plots_lg


## tables
crude$table <- crude$table + scale_y_discrete(labels = c("No", "Fluv.")) #theme(axis.text.y.left = element_text(angle=90, hjust=0.5, size = 10))
ipw$table <- ipw$table + scale_y_discrete(labels = c("No", "Fluv.")) #theme(axis.text.y.left = element_text(angle=90, hjust=0.5, size = 10))
outs$table <- outs$table + scale_y_discrete(labels = c("No", "Fluv.")) #theme(axis.text.y.left = element_text(angle=90, hjust=0.5, size = 10))

mort_tb_lg <- plot_grid(crude$table, ipw$table, outs$table, nrow=1)

## combine 

plot_grid(mort_plots_lg, mort_tb_lg, ncol=1, rel_heights=c(1, 0.5))


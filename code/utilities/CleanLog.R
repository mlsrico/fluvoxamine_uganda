CleanLog <- function(model){
      res_model <- model %>% summary 
      ci_model <- exp(confint(model))
      rname <- res_model$coefficients %>%
            as.data.frame %>% 
            rownames_to_column %>% 
            filter(rowname!="(Intercept)") %>% 
            pull(rowname)
      
      # aor
      aor <- res_model$coefficients %>%
            as.data.frame %>% 
            rownames_to_column %>% 
            filter(rowname!="(Intercept)") %>% 
            mutate(Estimate = round(exp(Estimate), 2)) %>% 
            pull(Estimate)
      
      # pval
      pval <- res_model$coefficients %>%
            as.data.frame %>% 
            rownames_to_column %>% 
            filter(rowname!="(Intercept)") %>% 
            rename(pval = `Pr(>|z|)`) %>% 
            mutate(pval = ifelse(pval>=0.01, 
                                 round(pval, 2), 
                                 ifelse(pval<0.001, 
                                        "<0.001", 
                                        round(pval, 3)))) %>% 
            pull(pval)
      
      # confint
      ci <- ci_model %>% 
            as.data.frame %>% 
            rownames_to_column() %>% 
            filter(rowname!="(Intercept)") %>% 
            rename(cinf = `2.5 %`) %>% 
            mutate(cinf = round(cinf, 2)) %>% 
            pull(cinf)
      
      cs <- ci_model %>% 
            as.data.frame %>% 
            rownames_to_column() %>% 
            filter(rowname!="(Intercept)") %>% 
            rename(csup = `97.5 %`) %>% 
            mutate(csup = round(csup, 2)) %>% 
            pull(csup)
      
      # result
      result <- paste(aor, " (", ci, "-", cs, "; ", pval, ")", sep = "")
      result <- data_frame(rn = rname, val = result)
      return(result)
}

CleanCox <- function(sres){
      sres <- summary(sres)
      rname <- sres$coefficients %>% as.data.frame() %>% rownames()
      
      hr <- sres$coefficients %>% as.data.frame() %>% 
            pull(`exp(coef)`) %>% round(.,2)
      pval <- sres$coefficients %>% as.data.frame() %>% 
            rename(pval = `Pr(>|z|)`) %>%
            mutate(pval = ifelse(pval>=0.01, 
                                 round(pval, 2), 
                                 ifelse(pval<0.001, 
                                        "<0.001", 
                                        round(pval, 3)))) %>% 
            pull(pval)
      
      ci <- sres$conf.int %>% as.data.frame() %>% 
            pull(`lower .95`) %>% round(., 2)
      cs <- sres$conf.int %>% as.data.frame() %>% 
            pull(`upper .95`) %>% round(., 2)
      
      result <- paste(hr, " (", ci, "-", cs, "; ", pval, ")", sep = "")
      result <- tibble(rn = rname, val = result)
      return(result)
}
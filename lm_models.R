# All possible interactions

interact_F_lm <- function(fit) {
  data_model <- fit$model
  
  formula_part1 <- format(formula(fit)) %>% paste(collapse = "")
  
  x_names <- names(data_model)[which(names(data_model) != "LNP_Percent")]
  
  pvalues_F <- matrix(nrow = length(x_names), ncol = length(x_names)) %>% as.data.frame()
  colnames(pvalues_F) <- x_names
  
  pvalues_F <- pvalues_F %>% 
    mutate(interact_var1 = x_names)
  
  
  for (i in 1:length(x_names)) {
    
    for (j in 1:length(x_names)) {
      
      if (i > j) {
        new_int = paste0(x_names[i], sep = ":", x_names[j])  
        
        formula = paste0(formula_part1, sep = " + ", new_int)
        
        mod <- lm(formula, data = data_model)
        
        # LR Test
        LR <- lmtest::lrtest(fit, mod)
        pval <- LR$`Pr(>Chisq)`[2]
        
        pvalues_F[i,j] <- pval
        
      }
      
    }
  }
  
  pvalues_F <- pvalues_F %>% 
    gather(key = "interact_var2", value = "pvalue", -interact_var1) %>% 
    filter(!is.na(pvalue))
  
  return(pvalues_F)
}

# Do for all years at once
all_years_int <- function(df) {
  
  # Set initial formula
  my_formula <- formula("LNP_Percent ~ .")
  
   # Continue until 5 interactions or break
  for(i in 1:100) {
    print(paste("Getting interaction", i))
    
    eachyear_int <- data.frame()
    
    for(j in 1:6) {
    
      time <- c("2001", "2004", "2007", "2010", "2013", "2016")[j]
      
      # Fit base model
      fit <- lm(my_formula, 
        data = df %>% filter(year == time) %>% dplyr::select(superset_vars, LNP_Percent))
      
      # Get next interactions
      eachyear_int <- bind_rows(eachyear_int,
        interact_F_lm(fit) %>%
        filter(pvalue < 0.01) %>% 
        mutate(year = time))
    }
    
    # Get top n
    add_int <- eachyear_int %>% 
      group_by(interact_var1, interact_var2) %>% 
      summarise(n = n(), minp = min(pvalue)) %>% 
      ungroup() %>% 
      top_n(n = 1, wt = n) %>% 
      filter(minp == min(minp)) %>% #incase there are multiple
      dplyr::select(interact_var1, interact_var2) %>% 
      unname() %>% unlist()
    
    if(is_empty(add_int)) {
      print(paste("Finished with interactions:", i-1))
      break
      }
    
    my_formula <- formula(
      paste0(
        paste(format(my_formula), collapse = ""), 
        " + ", add_int[1],":",add_int[2]))
  }
  return(my_formula)
}

test <- all_years_int(small_df)

test %>% format(formula()) %>% paste(collapse="")

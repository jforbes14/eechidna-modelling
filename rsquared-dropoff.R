# Looking at R-squared increase for adding an additional variable to justify all 5 variable linear 
# models.

# ------------------------------------------------------------------------------------

## Function that fits all +1 variable models for top 10 AIC and a random 10

# Idea: fit all 1 variable, initialize with 2 variable (because doesn't use too much power)
# then choose top 10 plus 10 other random models and fit all possible +1 models. iterate up to 52.

step_fn <- function(y, x) {
  
  # Set up data frame for step models
  final_models <- data.frame(matrix(NA, nrow = 0, ncol = 7))
  colnames(final_models) = c("iter", "num_vars", "vars", "varname", "AIC", "r2", "adj_r2")
  
  # One variable models
  one_models = data.frame(matrix(NA, nrow = 32, ncol = 7))
  colnames(one_models) = c("iter", "num_vars", "vars", "varname", "AIC", "r2", "adj_r2")
  one_models$num_vars = 1
  
  for (k in 1:nrow(one_models)) {
    dat = cbind(y, x[, k])
    mod <- lm(LNP_Percent ~ ., data = dat)
    
    one_models$vars[k] = list(k)
    one_models$varname[k] = list(names(x)[k])
    one_models$AIC[k] = AIC(mod)
    one_models$r2[k] = summary(mod)$r.squared
    one_models$adj_r2[k] = summary(mod)$adj.r.squared
    one_models$iter[k] = k
  }
  
  # Add to final models
  final_models <- bind_rows(final_models, one_models)
  
  
  
  # Two variable models (start subsets)
  
  combos_2 = combn(1:ncol(x), 2)
  
  two_models = data.frame(matrix(NA, nrow = ncol(combos_2), ncol = 7))
  colnames(two_models) = c("iter", "num_vars", "vars", "varname", "AIC", "r2", "adj_r2")
  two_models$num_vars = 2
  
  for (k in 1:nrow(two_models)) {
    index = combos_2[,k]
    dat = cbind(y, x[, index])
    mod <- lm(LNP_Percent ~ ., data = dat)
    
    two_models$vars[k] = list(index)
    two_models$varname[k] = list(names(x)[index])
    two_models$AIC[k] = AIC(mod)
    two_models$r2[k] = summary(mod)$r.squared
    two_models$adj_r2[k] = summary(mod)$adj.r.squared
    two_models$iter[k] = k
  }
  
  # Take the top 10 models with highest r2
  top_models <- two_models %>% 
    arrange(-r2) %>% 
    top_n(10, wt = r2) %>% 
    mutate(type = "Top")
  
  # Take a random 10
  set.seed(123)
  sample_models <- two_models %>% 
    filter(!iter %in% top_models$iter) %>% 
    sample_n(size = 10) %>% 
    mutate(type = "Random")
  
  # Combine 
  subset_models <- bind_rows(top_models, sample_models)
  final_models <- bind_rows(final_models, subset_models)
  
  # Now iterate for up to 32 variable models
  while (max(final_models$num_vars) < 32) {
    
    # Previous number of variables
    nvar = max(final_models$num_vars)
    print(nvar)
    
    # Take only the 20 models from previous iteration
    subset_models <- final_models %>% filter(num_vars == nvar)
    
    # Set up data frame to hold all +1 variable models
    nmod = nrow(subset_models)*(ncol(x) - subset_models$num_vars[1])
    all_models = data.frame(matrix(NA, nrow = nmod, ncol = 7))
    colnames(all_models) = c("iter", "num_vars", "vars", "varname", "AIC", "r2", "adj_r2")
    all_models$num_vars = subset_models$num_vars[1] + 1
    
    # Initialize iter
    iter = 0
    
    # Loop
    for (k in 1:nrow(subset_models)) {
      vars = subset_models$vars[k] %>% unlist
      add_vars = (1:32)[-vars]
      
      for (j in add_vars) {
        iter = iter + 1
        
        index = c(vars,j)
        dat = cbind(y, x[, index])
        mod <- lm(LNP_Percent ~ ., data = dat)
        
        all_models$vars[iter] = list(index)
        all_models$varname[iter] = list(names(x)[index])
        all_models$AIC[iter] = AIC(mod)
        all_models$r2[iter] = summary(mod)$r.squared
        all_models$adj_r2[iter] = summary(mod)$adj.r.squared
        all_models$iter[iter] = iter
        
      }
      
    }
    
    # Filter out repeat models
    all_models <- all_models %>% 
      group_by(AIC) %>% 
      top_n(n = 1, wt = iter) %>% 
      ungroup()
    
    # Take the top 10 models with highest r2
    top_models <- all_models %>% 
      arrange(-r2) %>% 
      top_n(10, wt = r2) %>% 
      mutate(type = "Top")
    
    # Take a random 10 / Take all remaining if less than 20
    
    if (nrow(all_models) >= 20) {
      set.seed(123 + nvar)
      sample_models <- all_models %>% 
        filter(!iter %in% top_models$iter) %>%
        sample_n(size = 10) %>% 
        mutate(type = "Random")
    } else {
      sample_models <- all_models %>% 
        filter(!iter %in% top_models$iter) %>%
        mutate(type = "Random")
    }
    
    
    # Combine 
    final_models <- bind_rows(final_models, top_models, sample_models)
    
  }
  
  return(final_models)
}  

# ------------------------------------------------------------------------------------

# Run for each election

step_2016 <- step_fn(
  y = small_df %>% filter(year == "2016") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2016") %>% dplyr::select(-c(LNP_Percent, year))
)

step_2013 <- step_fn(
  y = small_df %>% filter(year == "2013") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2013") %>% dplyr::select(-c(LNP_Percent, year))
)

step_2010 <- step_fn(
  y = small_df %>% filter(year == "2010") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2010") %>% dplyr::select(-c(LNP_Percent, year))
)

step_2007 <- step_fn(
  y = small_df %>% filter(year == "2007") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2007") %>% dplyr::select(-c(LNP_Percent, year))
)

step_2004 <- step_fn(
  y = small_df %>% filter(year == "2004") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2004") %>% dplyr::select(-c(LNP_Percent, year))
)

step_2001 <- step_fn(
  y = small_df %>% filter(year == "2001") %>% dplyr::select(LNP_Percent),
  x = small_df %>% filter(year == "2001") %>% dplyr::select(-c(LNP_Percent, year))
)

# Combine and save

step_all <- bind_rows(step_2016 %>% mutate(year = "2016"),
  step_2013 %>% mutate(year = "2013"),
  step_2010 %>% mutate(year = "2010"),
  step_2007 %>% mutate(year = "2007"),
  step_2004 %>% mutate(year = "2004"),
  step_2001 %>% mutate(year = "2001")) %>% 
  group_by(num_vars, year) %>% 
  mutate(max_r2 = ifelse(r2 == max(r2), 1, 0))

save(step_all, file = "data/step_all.rda")


# ------------------------------------------------------------------------------------

# Plot

ggplot(aes(x = num_vars, y = r2), data = step_all) + 
  geom_point(alpha = 0.5) +
  geom_line(data = step_all %>% filter(max_r2 == 1)) +
  facet_wrap(~year)

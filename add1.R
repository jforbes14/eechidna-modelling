# Test all single variable additions to see if any are highly significant
my_model <- mod16
sp_weights = sp_weights_16
my_data = small_df %>% filter(year == "2016")

add1_sp <- function(my_model, sp_weights, my_data) {
  # Get all covariate names
  my_names <- names(my_model$coefficients)
  # Get all main effect names
  my_ints <- my_names[!grepl(":", my_names)][-1]
  # Response variable name
  resp_var <- strsplit((my_model$call %>% as.character())[2], " ~")[[1]][1]
  
  # Get only additional data
  add_data <- my_data %>% 
    dplyr::select_if(is.numeric) %>% 
    dplyr::select(-c(my_ints, resp_var))
  
  # Only modelled data
  mod_data <- my_data %>% 
    dplyr::select(c(my_ints, resp_var))
  
  # Create blank df
  df_add1 <- data.frame(VariableName = names(add_data), p = 0)
  
  # Add 1
  for (i in 1:nrow(df_add1)) {
    add_var = df_add$VariableName[i]
    use_df <- bind_cols(mod_data, add_data %>% select(add_var))
    
    add1_model <- errorsarlm(formula = my_model$call, listw = sp_weights_16, data = use_df)
    
    test <- lmtest::lrtest(add1_model, my_model)
    
    df_add1$p[i] <- test$`Pr(>Chisq)`[2]
    
  }
  return(df_add1)
}

# Run on all years
add1_allyears <- bind_rows(
  add1_sp(mod16, sp_weights_16, small_df %>% filter(year == "2016")) %>% mutate(year = "2016"),
  add1_sp(mod13, sp_weights_13, small_df %>% filter(year == "2013")) %>% mutate(year = "2013"),
  add1_sp(mod10, sp_weights_10, small_df %>% filter(year == "2010")) %>% mutate(year = "2010"),
  add1_sp(mod07, sp_weights_07, small_df %>% filter(year == "2007")) %>% mutate(year = "2007"),
  add1_sp(mod04, sp_weights_04, small_df %>% filter(year == "2004")) %>% mutate(year = "2004"),
  add1_sp(mod01, sp_weights_01, small_df %>% filter(year == "2001")) %>% mutate(year = "2001")
)

add1_allyears %>% 
  ggplot(aes(x = as.numeric(year), y = log(p))) + geom_point(aes(col = factor(p < 0.05))) + geom_line(aes(group = VariableName)) + facet_wrap(~VariableName, scales = "free") + guides(col = F)

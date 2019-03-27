
# GLS model (FGLS)
library(nlme)
my_formula <- "LNP_Percent ~ ."
my_data <- model_df %>% filter(year == "2016")
sp_weights <- sp_weights_16

# Function for FGLS
my_fgls <- function(my_formula, my_data, sp_weights) {
  # DivisionNm
  model_data <- my_data %>% dplyr::select(-c(DivisionNm, year))
  
  # Spatial weights matrix
  w_mat <- listw2mat(sp_weights)
  
  # Get OLS residuals
  ols_model <- lm(my_formula, model_data)
  my_res <- ols_model$residuals
  
  # Solve for rho
  res_model <- lm(my_res ~ w_mat%*%my_res)
  rho <- res_model$coefficients[2]
  rho_df <- data.frame(estimate = rho, se = summary(res_model)$coefficients[2,2], p = summary(res_model)$coefficients[2,4])
  
  # Transform data for GLS
  trans_mat <- diag(nrow(model_data)) - rho*w_mat
  gls_data <- data.frame(LNP_Percent = trans_mat %*% model_data$LNP_Percent,
    Intercept = trans_mat %*% rep(1,nrow(model_data))) %>% 
    bind_cols(as.data.frame(trans_mat %*% as.matrix(model_data %>% dplyr::select(-LNP_Percent))))
  
  # GLS model
  my_formula <- formula(paste0(my_formula,  " - 1"))
  gls_model <- gls(my_formula, gls_data)
  
  # Call to function with stargazer
  gls_model$call$model <- formula(paste0("LNP_Percent ~ ", paste0(names(gls_data)[-1], collapse = " + ")))
  
  # Rho and data
  gls_model$rho_df <- rho_df
  gls_model$gls_data <- gls_data
  gls_model$my_data <- my_data
  gls_model$actual_residuals <- solve(trans_mat)%*%gls_model$residuals
  
  return(gls_model)
}


# --------------------------------------------------------------------------------------------------

## Run full models for each year
full_formula = "LNP_Percent ~ ."

# 2016
glsmod16 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2016"),
  sp_weights = sp_weights_16)

# 2013
glsmod13 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2013"),
  sp_weights = sp_weights_13)

# 2010
glsmod10 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2010"),
  sp_weights = sp_weights_10)

# 2007
glsmod07 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2007"),
  sp_weights = sp_weights_07)

# 2004
glsmod04 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2004"),
  sp_weights = sp_weights_04)

# 2001
glsmod01 <- my_fgls(full_formula, 
  my_data = model_df %>% filter(year == "2001"),
  sp_weights = sp_weights_01)

# --------------------------------------------------------------------------------------------------

## Visualise coefficients and significance

coef_df <- bind_rows(
  data.frame(variable = glsmod16$coefficients %>% names, estimate = glsmod16$coefficients %>% unname, se = summary(glsmod16)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod16)$tTable[, "p-value"] %>% unname, year = 2016),
  data.frame(variable = glsmod13$coefficients %>% names, estimate = glsmod13$coefficients %>% unname, se = summary(glsmod13)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod13)$tTable[, "p-value"] %>% unname, year = 2013),
  data.frame(variable = glsmod10$coefficients %>% names, estimate = glsmod10$coefficients %>% unname, se = summary(glsmod10)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod10)$tTable[, "p-value"] %>% unname, year = 2010),
  data.frame(variable = glsmod07$coefficients %>% names, estimate = glsmod07$coefficients %>% unname, se = summary(glsmod07)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod07)$tTable[, "p-value"] %>% unname, year = 2007),
  data.frame(variable = glsmod04$coefficients %>% names, estimate = glsmod04$coefficients %>% unname, se = summary(glsmod04)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod04)$tTable[, "p-value"] %>% unname, year = 2004),
  data.frame(variable = glsmod01$coefficients %>% names, estimate = glsmod01$coefficients %>% unname, se = summary(glsmod01)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod01)$tTable[, "p-value"] %>% unname, year = 2001)
)

coef_df %>% 
  filter(variable != "Intercept") %>% 
  mutate(upper95 = estimate + 1.96*se, lower95 = estimate - 1.96*se) %>% 
  ggplot() +
  geom_point(aes(x = year, y = estimate, col = factor(p < 0.05)), size = 3) +
  geom_linerange(aes(x = year, ymin = lower95, ymax = upper95, col = factor(p < 0.05)), size = 1.5) + 
  geom_hline(aes(yintercept = 0), alpha = 0.5, size = 1) + 
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(values = c("grey50", "black"))

# --------------------------------------------------------------------------------------------------

## Visualise correlation parameter - only significant in 2001 and 2016

rho_df <- bind_rows(glsmod16$rho_df %>% mutate(year = "2016"), 
  glsmod13$rho_df %>% mutate(year = "2013"),
  glsmod10$rho_df %>% mutate(year = "2010"), 
  glsmod07$rho_df %>% mutate(year = "2007"),
  glsmod04$rho_df %>% mutate(year = "2004"), 
  glsmod01$rho_df %>% mutate(year = "2001")) %>% 
  mutate(upper95 = estimate + 1.96*se, lower95 = estimate - 1.96*se)

rho_df %>% ggplot() + 
  geom_point(aes(x = year, y = estimate, col = factor(p < 0.05)), size = 3) +
  geom_linerange(aes(x = year, ymin = lower95, ymax = upper95, col = factor(p < 0.05)), size = 1.5) + 
  geom_hline(aes(yintercept = 0), alpha = 0.5, size = 1) +
  scale_color_manual(values = c("grey50", "black"))

## Spatial correlation in GLS residuals - none

moran.test(glsmod16$residuals, sp_weights_16)
moran.test(glsmod13$residuals, sp_weights_13)
moran.test(glsmod10$residuals, sp_weights_10)
moran.test(glsmod07$residuals, sp_weights_07)
moran.test(glsmod04$residuals, sp_weights_04)
moran.test(glsmod01$residuals, sp_weights_01)


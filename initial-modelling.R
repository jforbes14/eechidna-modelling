# Loading the entire dataset
load("data/small_df.rda")

# ------------------------------------------------------------------------------------

# Ignoring the presence of spatial autocorrelation will cause OLS estimates to be inefficient, and the standard errors to be biased - but will not biased the coefficient estimates.

# Able to proceed with all 5 way linear models 

# ------------------------------------------------------------------------------------

# Basic models - without any variable selection

# Spatial model
sp_model16 <- errorsarlm(LNP_Percent ~ ., data=(small_df %>% filter(year == "2016") %>% select(-year)), sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

# Regression
lin_model16 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2016") %>% select(-year))

# Test for spatial correlation in residuals
moran.test(residuals(lin_model16), sp_weights_16)

# See which electorates differ the most from their neighbours (in terms of residuals)
moran.plot(residuals(lin_model16), sp_weights_16)

# ------------------------------------------------------------------------------------



# Testing proc time

# All 3 variable linear models
a <- proc.time()
test <- all_X_way(y_vec = small_df %>% filter(year == "2016") %>% select(LNP_Percent) %>% unlist %>% unname, 
  x_df = small_df %>% filter(year == "2016") %>% select(-c(LNP_Percent,year)),
  n_vars = 3)
b <- proc.time()
b-a

# All 3 variable spatial error models
library(rgeos)
library(spdep)
sF_16 <- sF_download(2016)
sF_13 <- sF_download(2013)
sF_10 <- sF_download(2010)
sF_07 <- sF_download(2007)
sF_04 <- sF_download(2004)
sF_01 <- sF_download(2001)
sp_weights_16 <- sp_weights_matrix(sF_16)
sp_weights_13 <- sp_weights_matrix(sF_13)
sp_weights_10 <- sp_weights_matrix(sF_10)
sp_weights_07 <- sp_weights_matrix(sF_07)
sp_weights_04 <- sp_weights_matrix(sF_04)
sp_weights_01 <- sp_weights_matrix(sF_01)


c <- proc.time()
test <- all_sperr_X_way(
  y_vec = small_df %>% filter(year == "2016") %>% select(LNP_Percent) %>% unlist %>% unname, 
  x_df = small_df %>% filter(year == "2016") %>% select(-c(LNP_Percent,year)),
  n_vars = 1,
  sp_weights = sp_weights_16)
d <- proc.time()
d-c
# Problem - all possible 5 variable spatial error models will take too long (~100 hours)

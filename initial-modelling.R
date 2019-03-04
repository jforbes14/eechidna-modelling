library(tidyverse)
library(spdep)

# Loading the entire dataset
load("data/small_df.rda")

# ------------------------------------------------------------------------------------

# Ignoring the presence of spatial autocorrelation will cause OLS estimates to be inefficient, and the standard errors to be biased - but will not biased the coefficient estimates.

# Able to proceed with all 5 way linear models 

# ------------------------------------------------------------------------------------

# Basic models - without any variable selection

# Spatial model
sp_model16 <- errorsarlm(LNP_Percent ~ ., data=(small_df %>% filter(year == "2016") %>% select(-c(DivisionNm, year))), sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

# Regression
lin_model16 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2016") %>% select(-c(DivisionNm, year)))

# Test for spatial correlation in residuals
moran.test(residuals(lin_model16), sp_weights_16)

# See which electorates differ the most from their neighbours (in terms of residuals)
moran.plot(residuals(lin_model16), sp_weights_16)

# ------------------------------------------------------------------------------------

# All 5 way linear models

# 2016
fiveway16 <- all_X_way(
  y_vec = small_df %>% filter(year == "2016") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2016") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway16, file = "data/fiveway16.rda")

# 2013
fiveway13 <- all_X_way(
  y_vec = small_df %>% filter(year == "2013") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2013") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway13, file = "data/fiveway13.rda")

# 2010
fiveway10 <- all_X_way(
  y_vec = small_df %>% filter(year == "2010") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2010") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway10, file = "data/fiveway10.rda")

# 2007
fiveway07 <- all_X_way(
  y_vec = small_df %>% filter(year == "2007") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2007") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway07, file = "data/fiveway07.rda")

# 2004
fiveway04 <- all_X_way(
  y_vec = small_df %>% filter(year == "2004") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2004") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway04, file = "data/fiveway04.rda")

# 2001
fiveway01 <- all_X_way(
  y_vec = small_df %>% filter(year == "2001") %>% select(LNP_Percent) %>% as.matrix(),
  x_df = small_df %>% filter(year == "2001") %>% select(-c(DivisionNm, LNP_Percent, year)),
  n_vars = 5
)

save(fiveway01, file = "data/fiveway01.rda")

# ------------------------------------------------------------------------------------

# Variable importance

varimp16 <- var_imp(Xway = fiveway16, x_df = small_df %>% filter(year == "2016") %>% select(-c(LNP_Percent, year)))
save(varimp16, file = "data/varimp16.rda")

varimp13 <- var_imp(Xway = fiveway13, x_df = small_df %>% filter(year == "2013") %>% select(-c(LNP_Percent, year)))
save(varimp13, file = "data/varimp13.rda")

varimp10 <- var_imp(Xway = fiveway10, x_df = small_df %>% filter(year == "2010") %>% select(-c(LNP_Percent, year)))
save(varimp10, file = "data/varimp10.rda")

varimp07 <- var_imp(Xway = fiveway07, x_df = small_df %>% filter(year == "2007") %>% select(-c(LNP_Percent, year)))
save(varimp07, file = "data/varimp07.rda")

varimp04 <- var_imp(Xway = fiveway04, x_df = small_df %>% filter(year == "2004") %>% select(-c(LNP_Percent, year)))
save(varimp04, file = "data/varimp04.rda")

varimp01 <- var_imp(Xway = fiveway01, x_df = small_df %>% filter(year == "2001") %>% select(-c(LNP_Percent, year)))
save(varimp01, file = "data/varimp01.rda")

# ------------------------------------------------------------------------------------

# Superset variables

# Function

superset_n <- function(topX) {
  bind_rows(
  varimp16 %>% top_n(topX, sum_w) %>% mutate(year = "2016"),
  varimp13 %>% top_n(topX, sum_w) %>% mutate(year = "2013"),
  varimp10 %>% top_n(topX, sum_w) %>% mutate(year = "2010"),
  varimp07 %>% top_n(topX, sum_w) %>% mutate(year = "2007"),
  varimp04 %>% top_n(topX, sum_w) %>% mutate(year = "2004"),
  varimp01 %>% top_n(topX, sum_w) %>% mutate(year = "2001")
) %>% 
  dplyr::select(varname, year) %>% 
  group_by(varname) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
}

# My superset

superset_vars <- superset_n(5) %>% 
  dplyr::select(varname) %>% 
  mutate(varname = as.character(varname)) %>% 
  unlist() %>% unname()

# ------------------------------------------------------------------------------------

# Test for spatial autocorrelation in the response

moran.test(small_df %>% filter(year == "2016") %>% select(LNP_Percent) %>% unlist, sp_weights_16)
moran.test(small_df %>% filter(year == "2013") %>% select(LNP_Percent) %>% unlist, sp_weights_13)
moran.test(small_df %>% filter(year == "2010") %>% select(LNP_Percent) %>% unlist, sp_weights_10)
moran.test(small_df %>% filter(year == "2007") %>% select(LNP_Percent) %>% unlist, sp_weights_07)
moran.test(small_df %>% filter(year == "2004") %>% select(LNP_Percent) %>% unlist, sp_weights_04)
moran.test(small_df %>% filter(year == "2001") %>% select(LNP_Percent) %>% unlist, sp_weights_01)

# Test for spatial autocorrelation in residuals of linear model without interactions

mod16_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2016") %>% select(c(LNP_Percent, superset_vars)))
mod13_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2013") %>% select(c(LNP_Percent, superset_vars)))
mod10_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2010") %>% select(c(LNP_Percent, superset_vars)))
mod07_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2007") %>% select(c(LNP_Percent, superset_vars)))
mod04_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2004") %>% select(c(LNP_Percent, superset_vars)))
mod01_lm <- lm(LNP_Percent ~ ., 
  data = small_df %>% filter(year == "2001") %>% select(c(LNP_Percent, superset_vars)))

moran.test(mod16_lm$residuals, sp_weights_16)
moran.test(mod13_lm$residuals, sp_weights_13)
moran.test(mod10_lm$residuals, sp_weights_10)
moran.test(mod07_lm$residuals, sp_weights_07)
moran.test(mod04_lm$residuals, sp_weights_04)
moran.test(mod01_lm$residuals, sp_weights_01)

# ------------------------------------------------------------------------------------

# Adding interactions (using spatial error model)

# Function to do F tests on possible addition of a single interaction

interact_F <- function(fit, sp_weights) {
  data_model <- data.frame(LNP_Percent = fit$y, fit$X[, -1]) 
  data_model <- data_model[ , !grepl("\\.", names(data_model))]
  
  formula_part1 <- (fit$call %>% as.character())[2]
  
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
        
        mod <- errorsarlm(
          formula, 
          data = data_model,
          sp_weights, 
          etype="error", method="eigen", interval=c(-1,0.999))
        
        # LR Test
        LR <- lmtest::lrtest(fit, mod)
        pval <- LR$`Pr(>Chisq)`[2]
        
        pvalues_F[i,j] <- pval
        
      }
      
    }
    print(i)
  }
  
  pvalues_F <- pvalues_F %>% 
    gather(key = "interact_var2", value = "pvalue", -interact_var1) %>% 
    filter(!is.na(pvalue))
  
  return(pvalues_F)
}

# 2016

mod16_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))
mod13_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999))
mod10_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999))
mod07_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999))
mod04_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999))
mod01_noint <- errorsarlm(LNP_Percent ~ ., 
  data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999))

# Models without interactions

int16 <- interact_F(mod16_noint, sp_weights_16) %>% mutate(year = "2016")
int13 <- interact_F(mod13_noint, sp_weights_13) %>% mutate(year = "2013") 
int10 <- interact_F(mod10_noint, sp_weights_10) %>% mutate(year = "2010") 
int07 <- interact_F(mod07_noint, sp_weights_07) %>% mutate(year = "2007") 
int04 <- interact_F(mod04_noint, sp_weights_04) %>% mutate(year = "2004") 
int01 <- interact_F(mod01_noint, sp_weights_01) %>% mutate(year = "2001") 

bind_rows(int01, int04, int07, int10, int13, int16) %>% 
  filter(pvalue < 0.01) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

## Add first interaction, and repeat

int16_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_16
)
int13_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_13
)
int10_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_10
)
int07_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_07
)
int04_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_04
)
int01_1 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive, 
    data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_01
)

bind_rows(int01_1, int04_1, int07_1, int10_1, int13_1, int16_1) %>% 
  filter(pvalue < 0.01) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

## Add second interaction, and repeat

int16_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_16
)
int13_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_13
)
int10_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_10
)
int07_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_07
)
int04_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_04
)
int01_2 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto, 
    data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_01
)

bind_rows(int01_2, int04_2, int07_2, int10_2, int13_2, int16_2) %>% 
  filter(pvalue < 0.001) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

## Adding third

int16_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_16
)
int13_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_13
)
int10_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_10
)
int07_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_07
)
int04_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_04
)
int01_3 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive, 
    data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_01
)

bind_rows(int01_3, int04_3, int07_3, int10_3, int13_3, int16_3) %>% 
  filter(pvalue < 0.01) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

# Fourth

int16_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_16
)
int13_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_13
)
int10_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_10
)
int07_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_07
)
int04_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_04
)
int01_4 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe, 
    data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_01
)

bind_rows(int01_4, int04_4, int07_4, int10_4, int13_4, int16_4) %>% 
  filter(pvalue < 0.01) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

# Fifth

int16_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_16
)
int13_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_13
)
int10_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_10
)
int07_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_07
)
int04_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_04
)
int01_5 <- interact_F(
  errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
    data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
    sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999)),
  sp_weights_01
)

bind_rows(int01_5, int04_5, int07_5, int10_5, int13_5, int16_5) %>% 
  filter(pvalue < 0.01) %>% 
  group_by(interact_var1, interact_var2) %>% 
  summarise(n = n(), minp = min(pvalue)) %>%
  ungroup() %>% 
  top_n(n = 1, wt = n) %>% 
  filter(minp == min(minp))

# No more interactions are significant at 1% level

# ------------------------------------------------------------------------------------

# Final models

mod16 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

mod13 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2013") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999))

mod10 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2010") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999))

mod07 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2007") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999))

mod04 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2004") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999))

mod01 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data=(small_df %>% filter(year == "2001") %>% dplyr::select(c(LNP_Percent, superset_vars))),
  sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999))

# View estimated coefficients
bind_rows(
  mod16$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2016", lambda = mod16$lambda),
  mod13$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2013", lambda = mod13$lambda),
  mod10$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2010", lambda = mod10$lambda),
  mod07$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2007", lambda = mod07$lambda),
  mod04$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2004", lambda = mod04$lambda),
  mod01$coefficients %>% t() %>% as.data.frame() %>% mutate(year = "2001", lambda = mod01$lambda)
) %>% gather(key = "variable", value = "value", -year) %>% 
  ggplot(aes(x=year, y=value)) +
  geom_line(aes(group = variable)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + facet_wrap(~variable, scales = "free")

# ------------------------------------------------------------------------------------

# Check for any spatial correlation in the residuals

moran.test(mod16$residuals, sp_weights_16)
moran.test(mod13$residuals, sp_weights_13)
moran.test(mod10$residuals, sp_weights_10)
moran.test(mod07$residuals, sp_weights_07)
moran.test(mod04$residuals, sp_weights_04)
moran.test(mod01$residuals, sp_weights_01)

# No evidence of spatial correlation in the residuals

moran.plot(mod16$residuals, sp_weights_16)

# ------------------------------------------------------------------------------------

# Misspecified model - spatial autocorrelation in residuals?
random_vars <- c("Born_SE_Europe", "ManagerAdminClericalSales", "Extractive", "RentLoanPrice", "OneParent_House", "MedianAge", "NoReligion", "Education", "Born_UK", "CurrentlyStudying")

mis_mod16 <- errorsarlm(LNP_Percent ~ . + MedianAge:Education + Extractive:ManagerAdminClericalSales + OneParent_House:Education + ManagerAdminClericalSales:Education, 
  data=(small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, random_vars))),
  sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

moran.test(mis_mod16$residuals, sp_weights_16)

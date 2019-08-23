## This script contains all of the modelling steps using the new SA1/CD imputed Census data, and includes
## the changes required to address ANZJS feedback.

## ------------------------------------------------------------------------------------------------ ##

# Data section

## ------------------------------------------------------------------------------------------------ ##

# Loading the entire dataset
library(eechidna)
library(tidyverse)
library(gridExtra)
library(nlme)

data(tpp01)
data(tpp04)
data(tpp07)
data(tpp10)
data(tpp13)
data(tpp16)
data(abs2001)
load("data/abs2004_cd.rda")
load("data/abs2007_cd.rda")
load("data/abs2010_cd.rda")
load("data/abs2013_cd.rda")
data(abs2016)

# Take log of religions/groups with outliers
abs2016 <- abs2016 %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))
abs2013_cd <- abs2013_cd %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))
abs2010_cd <- abs2010_cd %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))
abs2007_cd <- abs2007_cd %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))
abs2004_cd <- abs2004_cd %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))
abs2001 <- abs2001 %>% mutate(Indigenous = log(Indigenous), Judaism = log(Judaism), Islam = log(Islam), Buddhism = log(Buddhism))

# Combine and standardize
my_df <- bind_rows(
  left_join(tpp01, standardise_vars(abs2001) %>% dplyr::select(-c(UniqueID, Area, ends_with("NS"))), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2001"),
  left_join(tpp04, standardise_vars(abs2004_cd), by = c("DivisionNm")) %>% mutate(year = "2004"),
  left_join(tpp07, standardise_vars(abs2007_cd), by = c("DivisionNm")) %>% mutate(year = "2007"),
  left_join(tpp10, standardise_vars(abs2010_cd), by = c("DivisionNm")) %>% mutate(year = "2010"),
  left_join(tpp13, standardise_vars(abs2013_cd), by = c("DivisionNm")) %>% mutate(year = "2013"),
  left_join(tpp16, standardise_vars(abs2016) %>% dplyr::select(-c(UniqueID, Area, ends_with("NS"))), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2016")
) %>% 
  mutate(year = factor(year)) %>% 
  dplyr::select(-c(Population, StateAb, LNP_Votes, ALP_Votes, ALP_Percent, TotalVotes,
    Swing, InternetUse, InternetAccess, EnglishOnly, Other_NonChrist, OtherChrist, Volunteer, EmuneratedElsewhere, UniqueID, Catholic, Anglican)) %>% 
  mutate(Band_00_19 = Age00_04 + Age05_14 + Age15_19,
    Band_20_34 = Age20_24 + Age25_34,
    Band_35_54 = Age35_44 + Age45_54,
    Band_55plus = Age55_64 + Age65_74 + Age75_84 + Age85plus) %>% 
  select(-c(starts_with("Age"), MedianAge)) 



## ------------------------------------------------------------------------------------------------ ##

pca_16 <- prcomp(my_df %>% filter(year == "2016") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_16, npcs = 24, type = "lines")
pca_13 <- prcomp(my_df %>% filter(year == "2013") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_13, npcs = 24, type = "lines")
pca_10 <- prcomp(my_df %>% filter(year == "2010") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_10, npcs = 24, type = "lines")
pca_07 <- prcomp(my_df %>% filter(year == "2007") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_07, npcs = 24, type = "lines")
pca_04 <- prcomp(my_df %>% filter(year == "2004") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_04, npcs = 24, type = "lines")
pca_01 <- prcomp(my_df %>% filter(year == "2001") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band"))))
screeplot(pca_01, npcs = 24, type = "lines")

## ------------------------------------------------------------------------------------------------ ##

# Do PCA on all
pca_all <- prcomp(my_df %>% dplyr::select(-c(LNP_Percent, year, DivisionNm, starts_with("Band")))) %>% orientPCs()
# Plot
plot_pc_all1 <- pca_all %>% 
  dplyr::select(Variable, PC1) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC1") + guides(col = F)

plot_pc_all2 <- pca_all %>% 
  dplyr::select(Variable, PC2) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC2") + guides(col = F)

plot_pc_all3 <- pca_all %>% 
  dplyr::select(Variable, PC3) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC3") + guides(col = F)

plot_pc_all4 <- pca_all %>% 
  dplyr::select(Variable, PC4) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC4") + guides(col = F)

grid.arrange(plot_pc_all1, plot_pc_all2, plot_pc_all3, plot_pc_all4, nrow = 2)


## ------------------------------------------------------------------------------------------------ ##

# Create final df for modelling

factors_df <- my_df %>% 
  mutate(Education = BachelorAbv + HighSchool + Professional + Finance - Laborer - Tradesperson - DipCert,
    FamHouseSize = FamilyRatio + AverageHouseholdSize + Couple_WChild_House - Couple_NoChild_House -
      SP_House,
    PropertyOwned = Owned + Mortgage - Renting - PublicHousing,
    RentLoanPrice = MedianRent + MedianLoanPay,
    Incomes = MedianFamilyIncome + MedianHouseholdIncome + MedianPersonalIncome,
    Unemployment = Unemployed - LFParticipation) %>% 
  dplyr::select(-c(BachelorAbv, HighSchool, Professional, Finance, Laborer, Tradesperson, DipCert, FamilyRatio,
    AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House, Owned, Mortgage, Renting,
    PublicHousing, MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome, MedianRent, 
    MedianLoanPay, Unemployed, LFParticipation)) 

## ------------------------------------------------------------------------------------------------ ##

# Now standardize factors

small_df2 <- bind_rows(
  factors_df %>% filter(year == "2001") %>% standardise_vars(),
  factors_df %>% filter(year == "2004") %>% standardise_vars(),
  factors_df %>% filter(year == "2007") %>% standardise_vars(),
  factors_df %>% filter(year == "2010") %>% standardise_vars(),
  factors_df %>% filter(year == "2013") %>% standardise_vars(),
  factors_df %>% filter(year == "2016") %>% standardise_vars()
)

# Order electorates in alphabetical order to match spatial matrix

model_df2 <- small_df2 %>% 
  arrange(year, DivisionNm) %>% 
  dplyr::select(order(colnames(.))) %>% 
  select(-Band_55plus)


# ------------------------------------------------------------------------------------

# Load spatial weights
load("data/sp_weights_16.rda")
load("data/sp_weights_13.rda")
load("data/sp_weights_10.rda")
load("data/sp_weights_07.rda")
load("data/sp_weights_04.rda")
load("data/sp_weights_01.rda")

library(spdep)

## Run full models for each year
full_formula = "LNP_Percent ~ ."

# 2016
glsmod16 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2016"),
  sp_weights = sp_weights_16)

# 2013
glsmod13 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2013"),
  sp_weights = sp_weights_13)

# 2010
glsmod10 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2010"),
  sp_weights = sp_weights_10)

# 2007
glsmod07 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2007"),
  sp_weights = sp_weights_07)

# 2004
glsmod04 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2004"),
  sp_weights = sp_weights_04)

# 2001
glsmod01 <- my_fgls(full_formula, 
  my_data = model_df2 %>% filter(year == "2001"),
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

## Plotting effects
coef_df %>% 
  group_by(variable) %>% 
  summarise(n = sum(p < 0.05)) %>% View


p1 <- grid_visreg("Extractive", top = TRUE)
p2 <- grid_visreg("DiffAddress")
p3 <- grid_visreg("ManagerAdmin")
p4 <- grid_visreg("Transformative")
p5 <- grid_visreg("Incomes")
p6 <- grid_visreg("Unemployment")

p7 <- grid_visreg("MedianAge", top = TRUE)
p8 <- grid_visreg("Judaism")
p9 <- grid_visreg("OtherLanguage")
p10 <- grid_visreg("BornAsia")
p11 <- grid_visreg("Education")
p12 <- grid_visreg("BornSEEuro")
p13 <- grid_visreg("DeFacto")


## ------------------------------------------------------------------------------------------------ ##

# Multicollinearity in resultant variable set

# Variance inflation factor (VIF)
# vif = 1/(1 - r2)

my_vif <- function(fgls_mod) {
  vif_df <- data.frame(variable = names(fgls_mod$coefficients)[-1], vif = 0)
  
  for (i in 1:length(vif_df$variable)) {
    myvar = vif_df$variable[i] %>% as.character()
    othervar = vif_df$variable[-i] %>% as.character()
    formula = paste(myvar, "~", paste(othervar, collapse = " + "))
    mymod <- lm(formula, data = fgls_mod$gls_data %>% select(-LNP_Percent))
    r2 = (summary(mymod))$r.squared
    vif = 1/(1-r2)
    
    vif_df$vif[i] = vif
  }
  
  return(vif_df)
}

# Plot VIF (using OLS)
my_vif(glsmod16) %>% mutate(year = "2016") %>% 
  bind_rows(my_vif(glsmod13) %>% mutate(year = "2013")) %>% 
  bind_rows(my_vif(glsmod10) %>% mutate(year = "2010")) %>% 
  bind_rows(my_vif(glsmod07) %>% mutate(year = "2007")) %>% 
  bind_rows(my_vif(glsmod04) %>% mutate(year = "2004")) %>% 
  bind_rows(my_vif(glsmod01) %>% mutate(year = "2001")) %>%
    ggplot(aes(x = variable, y = vif, col = year)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))


## ------------------------------------------------------------------------------------------------ ##

# Raw variable scales and how they compare to the effect

# Eyeballing the large estimated coefficients and their min/max - seems fine

## ------------------------------------------------------------------------------------------------ ##

# Model with significant effects only

sig_vars <- coef_df %>% 
  group_by(variable) %>% 
  summarise(n = sum(p < 0.05)) %>% 
  filter(n > 0, variable != "Intercept") %>% 
  select(variable) %>% unlist %>% unname %>% as.character()

# Subset with only significant variables
subset_df <- model_df2 %>% 
  select(sig_vars, year, DivisionNm, LNP_Percent)

# 2016
glsmod16_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2016"),
  sp_weights = sp_weights_16)

# 2013
glsmod13_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2013"),
  sp_weights = sp_weights_13)

# 2010
glsmod10_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2010"),
  sp_weights = sp_weights_10)

# 2007
glsmod07_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2007"),
  sp_weights = sp_weights_07)

# 2004
glsmod04_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2004"),
  sp_weights = sp_weights_04)

# 2001
glsmod01_subset <- my_fgls(full_formula, 
  my_data = subset_df %>% filter(year == "2001"),
  sp_weights = sp_weights_01)

## Visualise coefficients and significance

coef_subset_df <- bind_rows(
  data.frame(variable = glsmod16_subset$coefficients %>% names, estimate = glsmod16_subset$coefficients %>% unname, se = summary(glsmod16_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod16_subset)$tTable[, "p-value"] %>% unname, year = 2016),
  data.frame(variable = glsmod13_subset$coefficients %>% names, estimate = glsmod13_subset$coefficients %>% unname, se = summary(glsmod13_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod13_subset)$tTable[, "p-value"] %>% unname, year = 2013),
  data.frame(variable = glsmod10_subset$coefficients %>% names, estimate = glsmod10_subset$coefficients %>% unname, se = summary(glsmod10_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod10_subset)$tTable[, "p-value"] %>% unname, year = 2010),
  data.frame(variable = glsmod07_subset$coefficients %>% names, estimate = glsmod07_subset$coefficients %>% unname, se = summary(glsmod07_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod07_subset)$tTable[, "p-value"] %>% unname, year = 2007),
  data.frame(variable = glsmod04_subset$coefficients %>% names, estimate = glsmod04_subset$coefficients %>% unname, se = summary(glsmod04_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod04_subset)$tTable[, "p-value"] %>% unname, year = 2004),
  data.frame(variable = glsmod01_subset$coefficients %>% names, estimate = glsmod01_subset$coefficients %>% unname, se = summary(glsmod01_subset)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod01_subset)$tTable[, "p-value"] %>% unname, year = 2001)
)

# Plot together
coef_subset_vis <- coef_df %>% 
  left_join(coef_subset_df %>% select(-c(se, p)) %>% rename(subset_est = estimate), 
    by = c("variable", "year")) %>% 
  filter(!is.na(subset_est))

coef_subset_vis %>% 
  filter(variable != "Intercept") %>% 
  mutate(upper95 = estimate + 1.96*se, lower95 = estimate - 1.96*se) %>% 
  ggplot() +
  geom_point(aes(x = year, y = estimate, col = factor(p < 0.05)), size = 3) +
  geom_linerange(aes(x = year, ymin = lower95, ymax = upper95, col = factor(p < 0.05)), size = 1.5) + 
  geom_point(aes(x = year, y = subset_est), col = "orange", size = 3) +
  geom_hline(aes(yintercept = 0), alpha = 0.5, size = 1) + 
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(values = c("grey50", "black"))


## ------------------------------------------------------------------------------------------------ ##

# Removing highest pairwise correlations and seeing if conclusions change
paircors <- matrix(model_df2 %>% 
  select(-c(LNP_Percent, year, DivisionNm)) %>%
  cor(), ncol = 32) %>% 
  data.frame()
colnames(paircors) <- model_df2 %>% 
  select(-c(LNP_Percent, year, DivisionNm)) %>% names
paircors$var <- model_df2 %>% 
  select(-c(LNP_Percent, year, DivisionNm)) %>% names

paircors_sig <- paircors %>% 
  gather("var2", "cor", -var) %>% 
  filter(var != var2) %>% 
  arrange(-cor)  %>% 
#  filter(var %in% sig_vars, var2 %in% sig_vars) %>% 
 # filter(abs(cor) > 0.5) %>% 
  top_n(n = 20, wt = abs(cor))

paircors_clean <- paircors_sig[seq(0, nrow(paircors_sig), 2),] %>% 
  arrange(var)

paircors_clean

## Function to see how coefficient changes
sig_cor_plot <- function(var1, var2) {
  # 2016
  glsmod16_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2016") %>% select(-var2),
    sp_weights = sp_weights_16)
  
  # 2013
  glsmod13_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2013") %>% select(-var2),
    sp_weights = sp_weights_13)
  
  # 2010
  glsmod10_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2010") %>% select(-var2),
    sp_weights = sp_weights_10)
  
  # 2007
  glsmod07_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2007") %>% select(-var2),
    sp_weights = sp_weights_07)
  
  # 2004
  glsmod04_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2004") %>% select(-var2),
    sp_weights = sp_weights_04)
  
  # 2001
  glsmod01_subset_rm <- my_fgls(full_formula, 
    my_data = model_df2 %>% filter(year == "2001") %>% select(-var2),
    sp_weights = sp_weights_01)
  
  # Coef from removed models
  coef_subset_rm_df <- bind_rows(
    data.frame(variable = glsmod16_subset_rm$coefficients %>% names, estimate = glsmod16_subset_rm$coefficients %>% unname, se = summary(glsmod16_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod16_subset_rm)$tTable[, "p-value"] %>% unname, year = 2016),
    data.frame(variable = glsmod13_subset_rm$coefficients %>% names, estimate = glsmod13_subset_rm$coefficients %>% unname, se = summary(glsmod13_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod13_subset_rm)$tTable[, "p-value"] %>% unname, year = 2013),
    data.frame(variable = glsmod10_subset_rm$coefficients %>% names, estimate = glsmod10_subset_rm$coefficients %>% unname, se = summary(glsmod10_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod10_subset_rm)$tTable[, "p-value"] %>% unname, year = 2010),
    data.frame(variable = glsmod07_subset_rm$coefficients %>% names, estimate = glsmod07_subset_rm$coefficients %>% unname, se = summary(glsmod07_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod07_subset_rm)$tTable[, "p-value"] %>% unname, year = 2007),
    data.frame(variable = glsmod04_subset_rm$coefficients %>% names, estimate = glsmod04_subset_rm$coefficients %>% unname, se = summary(glsmod04_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod04_subset_rm)$tTable[, "p-value"] %>% unname, year = 2004),
    data.frame(variable = glsmod01_subset_rm$coefficients %>% names, estimate = glsmod01_subset_rm$coefficients %>% unname, se = summary(glsmod01_subset_rm)$tTable[, "Std.Error"] %>% unname, p = summary(glsmod01_subset_rm)$tTable[, "p-value"] %>% unname, year = 2001)
  )
  
  # DF
  mydf = coef_df %>% 
    filter(variable == var1) %>% 
    mutate(rm_estimate = coef_subset_rm_df %>% filter(variable == var1) %>% select(estimate) %>% unlist,
      rm_se = coef_subset_rm_df %>% filter(variable == var1) %>% select(se) %>% unlist)
  
  # plot
  myplot <- mydf %>% 
    ggplot(aes(x = year)) +
    geom_point(aes(y = estimate), col = "blue") + 
    geom_point(aes(y = rm_estimate), col = "red") +
    geom_linerange(aes(ymin = rm_estimate - 1.96*rm_se, ymax = rm_estimate + 1.96*rm_se), col = "red", size = 0.5, alpha = 0.5) +
    geom_linerange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), col = "blue", size = 1, alpha = 0.5) +
    lims(y = c(-25,25))
  
  print(paste("Blue is effect for ", var1, " in full model with ", var2))
  print(paste("Red is effect for ", var1, " in model without ", var2))
  
  return(myplot)
}

# A quick analysis of the top 5 pairwise correlations amongst significant variables
# All of the 95% confidence intervals overlap
# No variable changes from significant in one direction to the other
sig_cor_plot("Band_20_34", "Married")
sig_cor_plot("Married", "Band_20_34")
sig_cor_plot("Born_SE_Europe", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "Born_SE_Europe")
sig_cor_plot("DiffAddress", "DeFacto")
sig_cor_plot("DeFacto", "DiffAddress")
sig_cor_plot("Education", "Incomes")
sig_cor_plot("Incomes", "Education")
sig_cor_plot("Incomes", "Unemployment")
sig_cor_plot("Unemployment", "Incomes")


# A quick analysis of the top 10 pairwise correlations amongst all variables
# All of the 95% confidence intervals overlap
# No variable changes from significant in one direction to the other
sig_cor_plot("AusCitizen", "Band_20_34")
sig_cor_plot("Band_20_34", "AusCitizen")
sig_cor_plot("AusCitizen", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "AusCitizen")
sig_cor_plot("BornElsewhere", "Born_Asia")
sig_cor_plot("Born_Asia", "BornElsewhere")
sig_cor_plot("Born_Asia", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "Born_Asia")
sig_cor_plot("Born_Asia", "Buddhism")
sig_cor_plot("Buddhism", "Born_Asia")
sig_cor_plot("BornElsewhere", "Buddhism")
sig_cor_plot("Buddhism", "BornElsewhere")
sig_cor_plot("BornElsewhere", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "BornElsewhere")
sig_cor_plot("DiffAddress", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "DiffAddress")
sig_cor_plot("RentLoanPrice", "Incomes")
sig_cor_plot("Incomes", "RentLoanPrice")
sig_cor_plot("Islam", "OtherLanguageHome")
sig_cor_plot("OtherLanguageHome", "Islam")

## ------------------------------------------------------------------------------------------------ ##

# Incumbent plots

# Incumbents
incumbent <- function(fp) {
  df <- fp %>% filter(HistoricElected == "Y", PartyAb == "LNP") %>% select(DivisionNm, HistoricElected)
  return(df)
}

# 2007-2016

incum_resids <- data.frame(
  Residuals = c(glsmod16$actual_residuals, glsmod13$actual_residuals, glsmod10$actual_residuals, glsmod07$actual_residuals, glsmod04$actual_residuals), 
  bind_rows(
    glsmod16$my_data %>% select(DivisionNm, year) %>% left_join(incumbent(fp16), by = "DivisionNm"),
    glsmod13$my_data %>% select(DivisionNm, year) %>% left_join(incumbent(fp13), by = "DivisionNm"),
    glsmod10$my_data %>% select(DivisionNm, year) %>% left_join(incumbent(fp10), by = "DivisionNm"),
    glsmod07$my_data %>% select(DivisionNm, year) %>% left_join(incumbent(fp07), by = "DivisionNm"),
    glsmod04$my_data %>% select(DivisionNm, year) %>% left_join(
      fp04 %>% filter(PartyAb == "LNP") %>% 
        left_join(
          fp01 %>% rename(HistoricElected = Elected) %>% select(UniqueID, HistoricElected, PartyAb), 
          by = c("UniqueID", "PartyAb")
        ) %>% incumbent() %>% unique(), by = "DivisionNm")) %>%
    mutate(Incumbent = ifelse(is.na(HistoricElected), "N", "Y")))

ggplot(aes(x = year, y = Residuals, col = Incumbent), data = incum_resids) + geom_boxplot()

# 2004 

fp04 %>% filter(PartyAb == "LNP") %>% 
  left_join(
  fp01 %>% rename(HistoricElected = Elected) %>% select(UniqueID, HistoricElected, PartyAb), 
    by = c("UniqueID", "PartyAb")
    ) %>% 
  incumbent()

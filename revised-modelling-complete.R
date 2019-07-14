## This script contains all of the modelling steps using the new SA1/CD imputed Census data, and includes
## the changes required to address ANZJS feedback.

## ------------------------------------------------------------------------------------------------ ##

# Data section

## ------------------------------------------------------------------------------------------------ ##

# Loading the entire dataset
library(eechidna)
library(tidyverse)

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
    Swing, InternetUse, InternetAccess, EnglishOnly, Other_NonChrist, OtherChrist, Volunteer, EmuneratedElsewhere, UniqueID, Catholic, Anglican)) 

## ------------------------------------------------------------------------------------------------ ##

# Group variables that are very highly correlated and probably describe the same information

# Incomes
cor(my_df %>% select(contains("Income")))

# Job type
cor(my_df %>% select(c(Professional, Finance)))
cor(my_df %>% select(c(Laborer, Tradesperson)))
cor(my_df %>% select(c(Distributive, Transformative)))
  
# Education
cor(my_df %>% select(c(BachelorAbv, HighSchool, DipCert, Professional)))

# Property ownership
cor(my_df %>% select(c(Owned, Renting)))

# Loan price
cor(my_df %>% select(c(MedianRent, MedianLoanPay)))


# Unemployment
cor(my_df %>% select(c(Unemployed, LFParticipation)))

# Family and house size
cor(my_df %>% select(c(FamilyRatio, AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House)))

# Age brackets
# 0-19, 20-44, 45-above are the three logical brackets because of correlations
cor(my_df %>% select(starts_with("Age"))) %>% View

# Removing other variables (if needed)
# Mortgage, 
  
my_df2 <- my_df %>% 
mutate(Education = BachelorAbv + HighSchool + Professional - DipCert,
  LaborerTrades = Laborer + Tradesperson,
  DistributeTransform = Distributive + Transformative,
  FamHouseSize = FamilyRatio + AverageHouseholdSize + Couple_WChild_House - Couple_NoChild_House -
    SP_House,
  PropertyOwned = Owned - Renting,
  RentLoanPrice = MedianRent + MedianLoanPay,
  Incomes = MedianFamilyIncome + MedianHouseholdIncome + MedianPersonalIncome,
  Unemployment = Unemployed - LFParticipation,
  Band_00_19 = Age00_04 + Age05_14 + Age15_19,
    Band_20_34 = Age20_24 + Age25_34,
    Band_35_54 = Age35_44 + Age45_54,
    Band_55plus = Age55_64 + Age65_74 + Age75_84 + Age85plus) %>% 
  select(-c(starts_with("Age"), MedianAge, Band_55plus, BachelorAbv, HighSchool, Laborer, Tradesperson, DipCert,
    FamilyRatio, AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House, Owned, Renting, 
    MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome, MedianRent, MedianLoanPay, Unemployed, 
    LFParticipation, Professional)) %>% 
  select(-c(BornElsewhere, PublicHousing, Mortgage, OtherLanguageHome, Distributive, Transformative))

# Now standardize factors

small_df2 <- bind_rows(
  my_df2 %>% filter(year == "2001") %>% standardise_vars(),
  my_df2 %>% filter(year == "2004") %>% standardise_vars(),
  my_df2 %>% filter(year == "2007") %>% standardise_vars(),
  my_df2 %>% filter(year == "2010") %>% standardise_vars(),
  my_df2 %>% filter(year == "2013") %>% standardise_vars(),
  my_df2 %>% filter(year == "2016") %>% standardise_vars()
)

# Order electorates in alphabetical order to match spatial matrix

model_df2 <- small_df2 %>% 
  arrange(year, DivisionNm) %>% 
  dplyr::select(order(colnames(.)))

save(model_df2, file = "data/model_df2.rda")

# Check for outliers
#abs2016 %>% select(contains("Media")) %>% gather("key", "value") %>% ggplot(aes(x = key, y = log(value))) + geom_jitter()


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
temp <- grid_visreg("OtherLanguage")
p10 <- grid_visreg("BornAsia")
p11 <- grid_visreg("Education")
p12 <- grid_visreg("BornSEEuro")
p13 <- grid_visreg("DeFacto")

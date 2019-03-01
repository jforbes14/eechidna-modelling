# Loading the entire dataset
library(eechidna)
library(tidyverse)
library(gridExtra)

data(tpp01)
data(tpp04)
data(tpp07)
data(tpp10)
data(tpp13)
data(tpp16)
data(abs2001)
data(abs2004)
data(abs2007)
data(abs2010)
data(abs2013)
data(abs2016)


abs2001 <- abs2001 %>%
  select(-starts_with("Age"))
abs2004 <- abs2004 %>% 
    select(-starts_with("Age"))
abs2007 <- abs2007 %>% 
    select(-starts_with("Age"))
abs2010 <- abs2010 %>% 
    select(-starts_with("Age"))
abs2013 <- abs2013 %>% 
    select(-starts_with("Age"))
abs2016 <- abs2016 %>% 
    select(-starts_with("Age"))

# Combine and standardize
my_df <- bind_rows(
  left_join(tpp01, standardise_vars(abs2001) %>% select(-c(ID, Area, ends_with("NS"), Population)), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2001"),
  left_join(tpp04, standardise_vars(abs2004), by = c("DivisionNm")) %>% mutate(year = "2004"),
  left_join(tpp07, standardise_vars(abs2007), by = c("DivisionNm")) %>% mutate(year = "2007"),
  left_join(tpp10, standardise_vars(abs2010), by = c("DivisionNm")) %>% mutate(year = "2010"),
  left_join(tpp13, standardise_vars(abs2013), by = c("DivisionNm")) %>% mutate(year = "2013"),
  left_join(tpp16, standardise_vars(abs2016) %>% select(-c(ID, Area, ends_with("NS"), Population)), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2016")
) %>% 
  select(-DivisionID) %>% 
  mutate(year = factor(year)) %>% 
  select(-c(DivisionNm, StateAb, LNP_Votes, ALP_Votes, ALP_Percent, TotalVotes,
    Swing, InternetUse, InternetAccess, EnglishOnly, Other_NonChrist, OtherChrist, Volunteer, EmuneratedElsewhere))

# Looking at correlations > 0.5
# temp <- my_df %>% select(-c(year, LNP_Percent)) %>% cor() %>% data.frame() %>% rownames_to_column(var = "var1") %>% gather(key = "var2", value = "corr", -var1) %>% filter(abs(corr) > 0.5, corr != 1)

# ------------------------------------------------------------------------------------

# Principal components

# Scree plots show a structural break after 4 PCs
pca_16 <- prcomp(my_df %>% filter(year == "2016") %>% select(-c(LNP_Percent, year)))
screeplot(pca_16, npcs = 24, type = "lines")
pca_13 <- prcomp(my_df %>% filter(year == "2013") %>% select(-c(LNP_Percent, year)))
screeplot(pca_13, npcs = 24, type = "lines")
pca_10 <- prcomp(my_df %>% filter(year == "2010") %>% select(-c(LNP_Percent, year)))
screeplot(pca_10, npcs = 24, type = "lines")
pca_07 <- prcomp(my_df %>% filter(year == "2007") %>% select(-c(LNP_Percent, year)))
screeplot(pca_07, npcs = 24, type = "lines")
pca_04 <- prcomp(my_df %>% filter(year == "2004") %>% select(-c(LNP_Percent, year)))
screeplot(pca_04, npcs = 24, type = "lines")
pca_01 <- prcomp(my_df %>% filter(year == "2001") %>% select(-c(LNP_Percent, year)))
screeplot(pca_01, npcs = 24, type = "lines")

# Combine and orient

pca_loadings <- bind_rows(
  pca_16 %>% orientPCs() %>% mutate(year = "2016"),
  pca_13 %>% orientPCs() %>% mutate(year = "2013"),
  pca_10 %>% orientPCs() %>% mutate(year = "2010"),
  pca_07 %>% orientPCs() %>% mutate(year = "2007"),
  pca_04 %>% orientPCs() %>% mutate(year = "2004"),
  pca_01 %>% orientPCs() %>% mutate(year = "2001")
)

# Plot
plot_pc1 <- pca_loadings %>% 
  select(Variable, PC1, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC1") + guides(col = F)

plot_pc2 <- pca_loadings %>% 
  select(Variable, PC2, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC2") + guides(col = F)

plot_pc3 <- pca_loadings %>% 
  select(Variable, PC3, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC3") + guides(col = F)

plot_pc4 <- pca_loadings %>% 
  select(Variable, PC4, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC4") + guides(col = F)

grid.arrange(plot_pc1, plot_pc2, plot_pc3, plot_pc4, nrow = 2)

# Do PCA on all
pca_all <- prcomp(my_df %>% select(-c(LNP_Percent, year))) %>% orientPCs()
# Plot
plot_pc_all1 <- pca_all %>% 
  select(Variable, PC1) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC1") + guides(col = F)

plot_pc_all2 <- pca_all %>% 
  select(Variable, PC2) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC2") + guides(col = F)

plot_pc_all3 <- pca_all %>% 
  select(Variable, PC3) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) +  
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC3") + guides(col = F)

plot_pc_all4 <- pca_all %>% 
  select(Variable, PC4) %>% 
  gather(key = "PC", value = "Loading", -c(Variable)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point(aes(col = (abs(Loading) > 0.15))) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC4") + guides(col = F)

grid.arrange(plot_pc_all1, plot_pc_all2, plot_pc_all3, plot_pc_all4, nrow = 2)

# Group variables

small_df <- my_df %>% 
  mutate(Education = BachelorAbv + HighSchool + Professional + Finance - Laborer - Tradesperson - DipCert,
    FamHouseSize = FamilyRatio + AverageHouseholdSize + Couple_WChild_House - Couple_NoChild_House - SP_House,
    PropertyOwned = Owned + Mortgage - Renting - PublicHousing,
    RentLoanPrice = MedianRent + MedianLoanPay,
    Incomes = MedianFamilyIncome + MedianHouseholdIncome + MedianPersonalIncome,
    Unemployment = Unemployed - LFParticipation) %>% 
  select(-c(BachelorAbv, HighSchool, Professional, Finance, Laborer, Tradesperson, DipCert, FamilyRatio, AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House, Owned, Mortgage, Renting, PublicHousing, MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome, MedianRent, MedianLoanPay, Unemployed, LFParticipation))


# ------------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------------

# Basic models

# Spatial model
sp_model16 <- errorsarlm(LNP_Percent ~ ., data=(small_df %>% filter(year == "2016") %>% select(-year)), sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

# Regression
lin_model16 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2016") %>% select(-year))
lin_model13 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2013") %>% select(-year))
lin_model10 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2010") %>% select(-year))
lin_model07 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2007") %>% select(-year))
lin_model04 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2004") %>% select(-year))
lin_model01 <- lm(LNP_Percent ~ ., small_df %>% filter(year == "2001") %>% select(-year))


# Test for spatial correlation in residuals
moran.test(residuals(lin_model16), sp_weights_16)
moran.test(residuals(lin_model13), sp_weights_13)
moran.test(residuals(lin_model10), sp_weights_10)
moran.test(residuals(lin_model07), sp_weights_07)
moran.test(residuals(lin_model04), sp_weights_04)
moran.test(residuals(lin_model01), sp_weights_01)

# See which electorates differ the most from their neighbours (in terms of residuals)
moran.plot(residuals(lin_model16), sp_weights_16)
moran.plot(residuals(lin_model13), sp_weights_13)
moran.plot(residuals(lin_model10), sp_weights_10)
moran.plot(residuals(lin_model07), sp_weights_07)
moran.plot(residuals(lin_model04), sp_weights_04)
moran.plot(residuals(lin_model01), sp_weights_01)

# Plot the residuals a

# ------------------------------------------------------------------------------------

# Alternative method of variable selection

# LASSO
lambda <- 10^seq(10, -2, length = 100)
library(glmnet)
x <- small_df %>% filter(year == "2016") %>% select(-c(year, LNP_Percent)) %>% as.matrix
y <- small_df %>% filter(year == "2016") %>% select(LNP_Percent) %>% as.matrix
lasso.mod <- glmnet(x, y, alpha = 1, lambda = lambda)
cv.out <- cv.glmnet(x, y, alpha = 1)
bestlam <- cv.out$lambda.min
predict(lasso.mod, type = "coefficients", s = bestlam)

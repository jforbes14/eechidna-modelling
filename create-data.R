# Combining the datasets and reducing the dimension by constructing factors

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
data(abs2004)
data(abs2007)
data(abs2010)
data(abs2013)
data(abs2016)

# Combine and standardize
my_df <- bind_rows(
  left_join(tpp01, standardise_vars(abs2001) %>% dplyr::select(-c(UniqueID, Area, ends_with("NS"), Population)), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2001"),
  left_join(tpp04, standardise_vars(abs2004) %>% dplyr::select(-UniqueID), by = c("DivisionNm")) %>% mutate(year = "2004"),
  left_join(tpp07, standardise_vars(abs2007) %>% dplyr::select(-UniqueID), by = c("DivisionNm")) %>% mutate(year = "2007"),
  left_join(tpp10, standardise_vars(abs2010) %>% dplyr::select(-UniqueID), by = c("DivisionNm")) %>% mutate(year = "2010"),
  left_join(tpp13, standardise_vars(abs2013) %>% dplyr::select(-UniqueID), by = c("DivisionNm")) %>% mutate(year = "2013"),
  left_join(tpp16, standardise_vars(abs2016) %>% dplyr::select(-c(UniqueID, Area, ends_with("NS"), Population)), by = c("DivisionNm", "StateAb"="State")) %>% mutate(year = "2016")
) %>% 
  mutate(year = factor(year)) %>% 
  dplyr::select(-c(starts_with("Age"), StateAb, LNP_Votes, ALP_Votes, ALP_Percent, TotalVotes,
    Swing, InternetUse, InternetAccess, EnglishOnly, Other_NonChrist, OtherChrist, Volunteer, EmuneratedElsewhere, UniqueID, Catholic, Anglican))

# ------------------------------------------------------------------------------------

# Principal components

# Scree plots show a structural break after 4 PCs
pca_16 <- prcomp(my_df %>% filter(year == "2016") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
screeplot(pca_16, npcs = 24, type = "lines")
pca_13 <- prcomp(my_df %>% filter(year == "2013") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
screeplot(pca_13, npcs = 24, type = "lines")
pca_10 <- prcomp(my_df %>% filter(year == "2010") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
screeplot(pca_10, npcs = 24, type = "lines")
pca_07 <- prcomp(my_df %>% filter(year == "2007") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
screeplot(pca_07, npcs = 24, type = "lines")
pca_04 <- prcomp(my_df %>% filter(year == "2004") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
screeplot(pca_04, npcs = 24, type = "lines")
pca_01 <- prcomp(my_df %>% filter(year == "2001") %>% dplyr::select(-c(LNP_Percent, year, DivisionNm)))
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
  dplyr::select(Variable, PC1, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC1") + guides(col = F)

plot_pc2 <- pca_loadings %>% 
  dplyr::select(Variable, PC2, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC2") + guides(col = F)

plot_pc3 <- pca_loadings %>% 
  dplyr::select(Variable, PC3, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC3") + guides(col = F)

plot_pc4 <- pca_loadings %>% 
  dplyr::select(Variable, PC4, year) %>% 
  gather(key = "PC", value = "Loading", -c(Variable, year)) %>%
  ggplot(aes(x = reorder(Variable, -Loading), y = Loading)) + 
  geom_point() + geom_line(aes(col = "orange", group = Variable)) + 
  theme(axis.text.x = element_text(angle = 60, size = 6, hjust = 1)) +
  labs(x = "Variable") + ggtitle("PC4") + guides(col = F)

library(gridExtra)
grid.arrange(plot_pc1, plot_pc2, plot_pc3, plot_pc4, nrow = 2)

# Do PCA on all
pca_all <- prcomp(my_df %>% dplyr::select(-c(LNP_Percent, year, DivisionNm))) %>% orientPCs()
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

# Now standardize factors

small_df <- bind_rows(
  factors_df %>% filter(year == "2001") %>% standardise_vars(),
  factors_df %>% filter(year == "2004") %>% standardise_vars(),
  factors_df %>% filter(year == "2007") %>% standardise_vars(),
  factors_df %>% filter(year == "2010") %>% standardise_vars(),
  factors_df %>% filter(year == "2013") %>% standardise_vars(),
  factors_df %>% filter(year == "2016") %>% standardise_vars()
)

# Order electorates in alphabetical order to match spatial matrix

model_df <- small_df %>% 
  arrange(year, DivisionNm)

save(model_df, file = "data/model_df.rda")


# ------------------------------------------------------------------------------------

# Get spatial weights
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

save(sp_weights_16, file = "data/sp_weights_16.rda")
save(sp_weights_13, file = "data/sp_weights_13.rda")
save(sp_weights_10, file = "data/sp_weights_10.rda")
save(sp_weights_07, file = "data/sp_weights_07.rda")
save(sp_weights_04, file = "data/sp_weights_04.rda")
save(sp_weights_01, file = "data/sp_weights_01.rda")
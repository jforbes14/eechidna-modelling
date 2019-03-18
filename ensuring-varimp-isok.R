#test if its ok

data(abs2016)

usethisdf <- abs2016 %>% 
  left_join(tpp16 %>% select(LNP_Percent, UniqueID), by = "UniqueID") %>% 
  mutate(Education = BachelorAbv + HighSchool + Professional + Finance - Laborer - Tradesperson - DipCert,
    FamHouseSize = FamilyRatio + AverageHouseholdSize + Couple_WChild_House - Couple_NoChild_House -
      SP_House,
    PropertyOwned = Owned + Mortgage - Renting - PublicHousing,
    RentLoanPrice = MedianRent + MedianLoanPay,
    Incomes = MedianFamilyIncome + MedianHouseholdIncome + MedianPersonalIncome,
    Unemployment = Unemployed - LFParticipation) %>% 
  select(-c(BachelorAbv, HighSchool, Professional, Finance, Laborer, Tradesperson, DipCert, FamilyRatio,
    AverageHouseholdSize, Couple_WChild_House, Couple_NoChild_House, SP_House, Owned, Mortgage, Renting,
    PublicHousing, MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome, MedianRent, 
    MedianLoanPay, Unemployed, LFParticipation)) %>% 
  standardise_vars()

mod <- lm(LNP_Percent ~ Extractive + ManagerAdminClericalSales + Born_SE_Europe + Unemployment + Incomes, data = usethisdf)

mod %>% summary
mod %>% AIC


temp <- whole_df %>% filter(year == 2016) %>% select(LNP_Percent, DivisionNm, Extractive)


############################################################################################

new_varimp <- bind_rows(varimp01 %>% mutate(year = 2001),
  varimp04 %>% mutate(year = 2004), 
  varimp07 %>% mutate(year = 2007), 
  varimp10 %>% mutate(year = 2010), 
  varimp13 %>% mutate(year = 2013), 
  varimp16 %>% mutate(year = 2016)
)

new_fiveway01 <- fiveway01
new_fiveway04 <- fiveway04
new_fiveway07 <- fiveway07
new_fiveway10 <- fiveway10
new_fiveway13 <- fiveway13
new_fiveway16 <- fiveway16

remove(fiveway01)
remove(fiveway04)
remove(fiveway07)
remove(fiveway10)
remove(fiveway13)
remove(fiveway16)

remove(varimp01)
remove(varimp04)
remove(varimp07)
remove(varimp10)
remove(varimp13)
remove(varimp16)



old_fiveway01 <- fiveway01
old_fiveway04 <- fiveway04
old_fiveway07 <- fiveway07
old_fiveway10 <- fiveway10
old_fiveway13 <- fiveway13
old_fiveway16 <- fiveway16

remove(fiveway01)
remove(fiveway04)
remove(fiveway07)
remove(fiveway10)
remove(fiveway13)
remove(fiveway16)

old_varimp <- bind_rows(varimp01 %>% mutate(year = 2001),
  varimp04 %>% mutate(year = 2004), 
  varimp07 %>% mutate(year = 2007), 
  varimp10 %>% mutate(year = 2010), 
  varimp13 %>% mutate(year = 2013), 
  varimp16 %>% mutate(year = 2016)
)
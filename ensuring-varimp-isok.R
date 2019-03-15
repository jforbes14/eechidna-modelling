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

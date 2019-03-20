model_df <- small_df %>% 
  select(-c(starts_with("Age"), "Anglican", "Catholic"))

# Full models for each year

fmod16 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2016") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

fmod13 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2013") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999))

fmod10 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2010") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999))

fmod07 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2007") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999))

fmod04 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2004") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999))

fmod01 <- errorsarlm(LNP_Percent ~ . + OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + ManagerAdminClericalSales:Extractive + OneParent_House:MedianAge, 
  data=(model_df %>% filter(year == "2001") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999))
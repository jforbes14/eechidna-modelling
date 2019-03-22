
# Full models for each year

fmod16 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2016") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_16, etype="error", method="eigen", interval=c(-1,0.999))

fmod13 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2013") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_13, etype="error", method="eigen", interval=c(-1,0.999))

fmod10 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2010") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_10, etype="error", method="eigen", interval=c(-1,0.999))

fmod07 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2007") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_07, etype="error", method="eigen", interval=c(-1,0.999))

fmod04 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2004") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_04, etype="error", method="eigen", interval=c(-1,0.999))

fmod01 <- errorsarlm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2001") %>% dplyr::select(-c(year, DivisionNm))),
  sp_weights_01, etype="error", method="eigen", interval=c(-1,0.999))

## Visualise coefficients and significance

coef_df <- bind_rows(
  data.frame(variable = fmod16$coefficients %>% names, estimate = fmod16$coefficients %>% unname, se = fmod16$rest.se %>% unname, p = summary(fmod16)$Coef[,4] %>% unname, year = 2016),
  data.frame(variable = fmod13$coefficients %>% names, estimate = fmod13$coefficients %>% unname, se = fmod13$rest.se %>% unname, p = summary(fmod13)$Coef[,4] %>% unname, year = 2013),
  data.frame(variable = fmod10$coefficients %>% names, estimate = fmod10$coefficients %>% unname, se = fmod10$rest.se %>% unname, p = summary(fmod10)$Coef[,4] %>% unname, year = 2010),
  data.frame(variable = fmod07$coefficients %>% names, estimate = fmod07$coefficients %>% unname, se = fmod07$rest.se %>% unname, p = summary(fmod07)$Coef[,4] %>% unname, year = 2007),
  data.frame(variable = fmod04$coefficients %>% names, estimate = fmod04$coefficients %>% unname, se = fmod04$rest.se %>% unname, p = summary(fmod04)$Coef[,4] %>% unname, year = 2004),
  data.frame(variable = fmod01$coefficients %>% names, estimate = fmod01$coefficients %>% unname, se = fmod01$rest.se %>% unname, p = summary(fmod01)$Coef[,4] %>% unname, year = 2001)
)

coef_df %>% 
  filter(variable != "(Intercept)") %>% 
  mutate(upper95 = estimate + 1.96*se, lower95 = estimate - 1.96*se) %>% 
  ggplot() +
  geom_point(aes(x = year, y = estimate, col = factor(p < 0.05)), size = 3) +
  geom_linerange(aes(x = year, ymin = lower95, ymax = upper95, col = factor(p < 0.05)), size = 1.5) + 
  geom_hline(aes(yintercept = 0), alpha = 0.5, size = 1) + 
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(values = c("grey50", "black"))

## Spatial correlation in residuals

moran.test(fmod16$residuals, sp_weights_16)
moran.test(fmod13$residuals, sp_weights_13)
moran.test(fmod10$residuals, sp_weights_10)
moran.test(fmod07$residuals, sp_weights_07)
moran.test(fmod04$residuals, sp_weights_04)
moran.test(fmod01$residuals, sp_weights_01)

my_res <- fmod16$tary - fmod16$tarX%*%fmod16$coefficients
scale_mat <- (diag(nrow(listw2mat(sp_weights_16))) - fmod16$lambda*listw2mat(sp_weights_16))
rev_res <- solve(scale_mat)%*%fmod16$residuals
moran.test(rev_res, sp_weights_16) # Confirmed spatial correlation in regular residuals

# GLS model
gls_data <- data.frame(LNP_Percent = fmod16$tary, fmod16$tarX[, -1])

names(gls_data) <- c("LNP_Percent", names(model_df %>% dplyr::select(-c(year, DivisionNm, LNP_Percent))))

gls_formula = paste0("LNP_Percent ~ ", paste0(names(model_df %>% dplyr::select(-c(year, DivisionNm, LNP_Percent))), collapse = " + "))

gls_model <- gls(LNP_Percent ~ AusCitizen + Born_Asia + Born_MidEast + Born_SE_Europe + Born_UK + BornElsewhere + Buddhism + Christianity + CurrentlyStudying + DeFacto + DiffAddress + Distributive + Extractive + Indigenous + Islam + Judaism + ManagerAdminClericalSales + Married + MedianAge + NoReligion + OneParent_House + OtherLanguageHome + SocialServ + Transformative + Education + FamHouseSize + PropertyOwned + RentLoanPrice + Incomes + Unemployment, data = gls_data)

# --------------------------------------------------------------------------------------------------

# OLS: Full models for each year

lmod16 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2016") %>% dplyr::select(-c(year, DivisionNm))))

lmod13 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2013") %>% dplyr::select(-c(year, DivisionNm))))

lmod10 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2010") %>% dplyr::select(-c(year, DivisionNm))))

lmod07 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2007") %>% dplyr::select(-c(year, DivisionNm))))

lmod04 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2004") %>% dplyr::select(-c(year, DivisionNm))))

lmod01 <- lm(LNP_Percent ~ ., 
  data=(model_df %>% filter(year == "2001") %>% dplyr::select(-c(year, DivisionNm))))

# Check for spatial correlation in residuals

moran.test(lmod16$residuals, sp_weights_16)
moran.test(lmod13$residuals, sp_weights_13)
moran.test(lmod10$residuals, sp_weights_10)
moran.test(lmod07$residuals, sp_weights_07)
moran.test(lmod04$residuals, sp_weights_04)
moran.test(lmod01$residuals, sp_weights_01)

# Only 2001 and 2016 show evidence of spatial correlation in residuals

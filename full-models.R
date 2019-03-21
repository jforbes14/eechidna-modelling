model_df <- small_df %>% 
  select(-c(starts_with("Age"), "Anglican", "Catholic"))

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
  filter(variable == "Unemployment") %>% 
  mutate(upper95 = estimate + 1.96*se, lower95 = estimate - 1.96*se) %>% 
  ggplot() +
  geom_point(aes(x = year, y = estimate, col = factor(p < 0.05)), size = 3) +
  geom_linerange(aes(x = year, ymin = lower95, ymax = upper95, col = factor(p < 0.05)), size = 1.5) + 
  geom_hline(aes(yintercept = 0), alpha = 0.5, size = 1) + 
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(values = c("grey50", "black"))


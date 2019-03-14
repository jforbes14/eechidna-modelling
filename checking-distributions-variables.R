# Checking distributions of variables

abs_all <- bind_rows(
  abs2001 %>% select(-UniqueID) %>% mutate(year = 2001),
  abs2004 %>% select(-UniqueID) %>% mutate(year = 2004),
  abs2007 %>% select(-UniqueID) %>% mutate(year = 2007),
  abs2010 %>% select(-UniqueID) %>% mutate(year = 2010),
  abs2013 %>% select(-UniqueID) %>% mutate(year = 2013),
  abs2016 %>% select(-UniqueID) %>% mutate(year = 2016)
) %>% select(-c(ends_with("NS"), DivisionNm, State, Area))

abs_all %>% select(SocialServ, Extractive, ManagerAdminClericalSales, Professional, year) %>%  gather(key = "variable", value = "value", -year) %>% ggplot(aes(x=factor(year), y=value, col = variable)) + geom_boxplot() + facet_wrap(~variable, scales = "free") + guides(col = F)

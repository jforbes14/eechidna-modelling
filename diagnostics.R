# Leverage
hatvals <- hat(fmod16$X[, -1])

ggplot(data = NULL) + geom_point(aes(x = small_df %>% filter(year == "2016") %>% dplyr::select(DivisionNm) %>% unlist, y = hatvals)) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(aes(yintercept = 3*(30/150)))

# QQ plots
library(ggplot2)
library(gridExtra)
qq16 <- ggplot(data = NULL, aes(sample = fmod16$residuals)) + geom_qq() + geom_qq_line() + title("2016")
qq13 <- ggplot(data = NULL, aes(sample = fmod13$residuals)) + geom_qq() + geom_qq_line() + title("2013")
qq10 <- ggplot(data = NULL, aes(sample = fmod10$residuals)) + geom_qq() + geom_qq_line() + title("2010")
qq07 <- ggplot(data = NULL, aes(sample = fmod07$residuals)) + geom_qq() + geom_qq_line() + title("2007")
qq04 <- ggplot(data = NULL, aes(sample = fmod04$residuals)) + geom_qq() + geom_qq_line() + title("2004")
qq01 <- ggplot(data = NULL, aes(sample = fmod01$residuals)) + geom_qq() + geom_qq_line() + title("2001")

grid.arrange(qq01, qq04, qq07, qq10, qq13, qq16, nrow = 2)

# Moran plots

# Predict one election ahead
pred16 <- predict(fmod13, model_df %>% filter(year == "2016") %>% dplyr::select(-c(DivisionNm, year)))
vote16 <- model_df %>% filter(year == "2016") %>% dplyr::select(LNP_Percent) %>% unlist
predict_res <- vote16 - pred16
ggplot(data = NULL, aes(x = predict_res)) + geom_density()

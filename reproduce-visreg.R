# Producing visreg style plots for SEM

# ------------------------------------------------------------------------------------------------

# Data
mydata <- model_df %>% filter(year == "2016") %>% dplyr::select(LNP_Percent, AusCitizen, Extractive, Unemployment)

# My spatial error model
my_model <- errorsarlm(LNP_Percent ~ AusCitizen + Extractive + Unemployment, data = model_df)

# ------------------------------------------------------------------------------------------------

## LINEAR MODEL EXAMPLE ##

# Linear model with visreglmod <- lm(LNP_Percent ~ ., data = mydata)
visreg(lmod, "Unemployment")

# Lambda matrix
u <- seq(-2.5,3.5,0.05)
lambda_mat <- matrix(c(`Intercept` = rep(1,length(u)), AusCitizen = rep(0,length(u)), Extractive = rep(0,length(u)), Unemployment = u), ncol = 4)

# X matrix
xmat <- mydata %>% 
  dplyr::select(-LNP_Percent) %>% 
  mutate(`(Intercept)` = 1) %>% 
  dplyr::select(`(Intercept)`, everything()) %>% 
  as.matrix()

# Beta matrix
beta <- lmod$coefficients %>%
  as.matrix()
beta_u <- beta[4]

# Standard error
sigma = ((lmod$residuals^2 %>% sum)/(150-4))^0.5

# T value
t = qt(0.975, 150-4)

# Confidence interval
plot_df <- data.frame(Unemployment = u, fitted = lambda_mat%*%beta, var = 0)

for (i in 1:nrow(lambda_mat)) {
  lambda <- lambda_mat[i, ]
  var[i] = sigma^2 * t(lambda) %*% solve(t(xmat)%*%xmat) %*% lambda
}

# Reproduce
plot_df <- plot_df %>% 
  mutate(upper95 = fitted + t*sqrt(var), lower95 = fitted - t*sqrt(var)) %>% 
  gather(key = "Bound", value = "Value", -c("Unemployment", var, fitted))

plot_visreg <- visreg(lmod, "Unemployment", gg = T, cond = list(AusCitizen = 0, Extractive = 0)) + lims(x = c(-2.5,3.5), y = c(25, 70)) + theme_bw()

plot_manual <- ggplot(data = plot_df) + geom_line(aes(x=Unemployment, y=Value, group = Bound)) + geom_line(aes(x = Unemployment, y = fitted), col = "blue") + geom_point(aes(x = x, y = y), data = plot_visreg$data, size = 0.5, col = "grey50") + lims(x = c(-2.5,3.5), y = c(25, 70)) + theme_bw()

grid.arrange(plot_manual, plot_visreg, nrow = 1)
  
# Producing visreg style plots for SEM

# ------------------------------------------------------------------------------------------------

# Data
mydata <- model_df %>% filter(year == "2016") %>% dplyr::select(LNP_Percent, AusCitizen, Extractive, Unemployment)

# My spatial error model
my_model <- errorsarlm(LNP_Percent ~ AusCitizen + Extractive + Unemployment, data = mydata,
  listw = sp_weights_16)
my_model <- fmod16
sp_weights <- sp_weights_16
varname = "Unemployment"

# Function to produce visreg style conditional plots
my_visreg <- function(my_model, sp_weights, varname, nolabs = FALSE, xlimits = NULL, ylimits = NULL) {
  # Extract fitted parameters
  rho <- my_model$lambda
  sigma <- (((my_model$y - my_model$X%*%my_model$coefficients)^2 %>% sum)/(length(my_model$y-my_model$parameters)))^0.5
  
  # Spatial weights
  w_mat <- listw2mat(sp_weights)
  
  # Q - where u = Qe, Q = (I - pW)^-1
  q_mat <- solve(diag(150) - rho*w_mat)
  
  # Omega - QQ'
  omega_mat <- q_mat%*%t(q_mat)
  
  # X
  x_mat <- my_model$X %>% 
    as.matrix()
  
  # Beta
  beta_mat <- my_model$coefficients
  
  # T value
  t = qt(0.975, 150-4)
  
  # Lambda matrix
  x <- round(seq(min(my_model$X[, varname])*1.2, max(my_model$X[, varname])*1.2, 0.05), 2)
  lambda_mat <- data.frame(matrix(0, nrow = length(x), ncol = ncol(my_model$X)))
  names(lambda_mat) <- names(my_model$X %>% as.data.frame)
  lambda_mat[, varname] <- x
  lambda_mat[, "(Intercept)"] <- 1
  lambda_mat <- as.matrix(lambda_mat)
  
  # Confidence interval
  plot_df <- data.frame(variable = x, fitted = lambda_mat%*%beta_mat, variance = 0)
  
  for (i in 1:nrow(lambda_mat)) {
    lambda <- lambda_mat[i, ]
    plot_df$variance[i] = sigma^2 * t(lambda) %*% 
      solve(t(x_mat) %*% solve(omega_mat) %*% x_mat) %*% 
      lambda
  }
  
  plot_df <- plot_df %>% 
    mutate(upper95 = fitted + t*sqrt(variance), lower95 = fitted - t*sqrt(variance))
  
  # Partial residuals
  points_df <- data.frame(
    variable = my_model$X[, varname],
    part_res = (my_model$y - my_model$X%*%my_model$coefficients) + as.matrix(my_model$X[, varname])%*%my_model$coefficients[varname] + my_model$coefficients[1]
  )
  
  # Plot
  myplot <- ggplot(data = plot_df) + 
    geom_ribbon(aes(x = variable, ymin = lower95, ymax = upper95), fill = "grey80") + 
    geom_point(aes(x = variable, y = part_res), data = points_df, size = 0.75, col = "grey50") + 
    geom_line(aes(x = variable, y = fitted), col = "blue", size = 1) +
    theme_bw() +
    labs(x = varname, y = "Response")
  
  if (nolabs == TRUE) {
    myplot <- myplot + labs(x = "", y = "")
  }
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    myplot <- myplot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(myplot)
}

# Test
my_visreg(fmod16, sp_weights_16, varname = "Extractive")

# ------------------------------------------------------------------------------------------------

## LINEAR MODEL EXAMPLE ##

# Linear model with visreglmod <- lm(LNP_Percent ~ ., data = mydata)
visreg(lmod, "Unemployment")

# Lambda matrix
u <- seq(-2.5,3.5,0.05)
lambda_mat <- matrix(c(`Intercept` = rep(1,length(u)), AusCitizen = rep(0,length(u)), Extractive = rep(0,length(u)), Unemployment = u), ncol = 4)

# X matrix
x_mat <- mydata %>% 
  dplyr::select(-LNP_Percent) %>% 
  mutate(`(Intercept)` = 1) %>% 
  dplyr::select(`(Intercept)`, everything()) %>% 
  as.matrix()

# Beta matrix
beta <- lmod$coefficients %>%
  as.matrix()
beta_u <- beta[4]

# Standard error
sigma = ((lmod$residuals^2 %>% sum)/(lmod$df.residual))^0.5

# T value
t = qt(0.975, 150-4)

# Confidence interval
plot_df <- data.frame(Unemployment = u, fitted = lambda_mat%*%beta, var = 0)

for (i in 1:nrow(lambda_mat)) {
  lambda <- lambda_mat[i, ]
  plot_df$var[i] = sigma^2 * t(lambda) %*% solve(t(x_mat)%*%x_mat) %*% lambda
}

# Reproduce
plot_df <- plot_df %>% 
  mutate(upper95 = fitted + t*sqrt(var), lower95 = fitted - t*sqrt(var)) %>% 
  gather(key = "Bound", value = "Value", -c("Unemployment", var, fitted))

plot_visreg <- visreg(lmod, "Unemployment", gg = T, cond = list(AusCitizen = 0, Extractive = 0)) + lims(x = c(-2.5,3.5), y = c(25, 70)) + theme_bw()

plot_manual <- ggplot(data = plot_df) + geom_line(aes(x=Unemployment, y=Value, group = Bound)) + geom_line(aes(x = Unemployment, y = fitted), col = "blue") + geom_point(aes(x = x, y = y), data = plot_visreg$data, size = 0.5, col = "grey50") + lims(x = c(-2.5,3.5), y = c(25, 70)) + theme_bw()

grid.arrange(plot_manual, plot_visreg, nrow = 1)
  
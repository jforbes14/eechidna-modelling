# Producing visreg style plots for SEM

# ------------------------------------------------------------------------------------------------

# Data
mydata <- model_df %>% filter(year == "2016") %>% dplyr::select(-c(year, DivisionNm, Extractive, Unemployment, CurrentlyStudying))

# My spatial error model
my_model <- errorsarlm(LNP_Percent ~ ., data = mydata,
  listw = sp_weights_16)
my_model <- fmod13
sp_weights <- sp_weights_13
varname = "FamHouseSize"

my_model <- glsmod16
sp_weights <- sp_weights_16

# Function to produce visreg style conditional plots
my_visreg <- function(my_model, sp_weights, varname, nolabs = FALSE, xlimits = NULL, ylimits = NULL) {
  # Extract fitted parameters
  rho <- my_model$rho_df$estimate
  sigma <- sqrt(sum(my_model$residuals^2)/(my_model$dims$N-my_model$dims$p))
  
  # Spatial weights
  w_mat <- listw2mat(sp_weights)
  
  # Q - where u = Qe, Q = (I - pW)^-1
  q_mat <- solve(diag(my_model$dims$N) - rho*w_mat)
  
  # Omega - QQ'
  omega_mat <- q_mat%*%t(q_mat)
  
  # X
  x_mat <- my_model$my_data %>% 
    dplyr::select(-LNP_Percent) %>% 
    mutate(Intercept = 1) %>% 
    select(Intercept, everything()) %>% 
    as.matrix()
  
  # Beta
  beta_mat <- my_model$coefficients
  
  # T value
  t = qt(0.975, nrow(my_model$gls_data)-ncol(my_model$gls_data))
  
  # Lambda matrix (FGLS)
  x <- round(seq(min(x_mat[, varname]), max(x_mat[, varname]), 0.025), 3)
  lambda_mat <- data.frame(matrix(0, nrow = length(x), ncol = ncol(x_mat)))
  names(lambda_mat) <- dimnames(x_mat)[[2]]
  lambda_mat[, varname] <- x
  lambda_mat$Intercept <- 1
  lambda_mat <- as.matrix(lambda_mat)
  
  # Lambda matrix
  #x <- round(seq(min(my_model$X[, varname]), max(my_model$X[, varname]), 0.025), 3)
  #lambda_mat <- data.frame(matrix(0, nrow = length(x), ncol = ncol(my_model$X)))
  #names(lambda_mat) <- names(my_model$X %>% as.data.frame)
  #lambda_mat[, varname] <- x
  #lambda_mat[, "(Intercept)"] <- 1
  #lambda_mat <- as.matrix(lambda_mat)
  
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
    variable = my_model$my_data[, varname] %>% unname,
    part_res = (my_model$my_data$LNP_Percent - x_mat%*%my_model$coefficients) + my_model$coefficients[varname]*x_mat[, varname] + my_model$coefficients[1]
  )
  
  # Plot
  myplot <- ggplot(data = plot_df) + 
    geom_ribbon(aes(x = variable, ymin = lower95, ymax = upper95), fill = "grey80") + 
    geom_point(aes(x = variable, y = part_res), data = points_df, size = 0.75, col = "grey50") + 
    geom_line(aes(x = variable, y = fitted), col = "blue", size = 1) +
    geom_hline(aes(yintercept = min(upper95)), col = "red") +
    geom_hline(aes(yintercept = max(lower95)), col = "blue") +
    theme_bw() + labs(x = varname, y = "Response")
  
  if (nolabs == TRUE) {
    myplot <- myplot + labs(x = "", y = "")
  }
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    myplot <- myplot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(myplot)
}

# Test
my_visreg(glsmod04, sp_weights_04, varname = "Born_MidEast")
my_visreg(glsmod13, sp_weights_13, varname = "Distributive")

# ------------------------------------------------------------------------------------------------

# OLD FUNCTION THAT WORKS WITH ERRORSARLM

# Function to produce visreg style conditional plots
old_visreg <- function(my_model, sp_weights, varname, nolabs = FALSE, xlimits = NULL, ylimits = NULL) {
  # Extract fitted parameters
  rho <- my_model$lambda
  #sigma <- (((my_model$y - my_model$X%*%my_model$coefficients)^2 %>% sum)/(length(my_model$y-my_model$parameters)))^0.5
  sigma <- sqrt(sum(my_model$residuals^2)/(nrow(my_model$X)-ncol(my_model$X)))
  
  # Spatial weights
  w_mat <- listw2mat(sp_weights)
  
  # Q - where u = Qe, Q = (I - pW)^-1
  q_mat <- solve(diag(nrow(my_model$X)) - rho*w_mat)
  
  # Omega - QQ'
  omega_mat <- q_mat%*%t(q_mat)
  
  # X
  x_mat <- my_model$X %>% 
    as.matrix()
  
  # Beta
  beta_mat <- my_model$coefficients
  
  # T value
  t = qt(0.975, nrow(my_model$X)-ncol(my_model$X))
  
  # Lambda matrix
  x <- round(seq(min(my_model$X[, varname]), max(my_model$X[, varname]), 0.025), 3)
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
    #geom_hline(aes(yintercept = min(upper95)), col = "red") +
    #geom_hline(aes(yintercept = max(lower95)), col = "purple") +
    theme_bw() + labs(x = varname, y = "Response")
  
  if (nolabs == TRUE) {
    myplot <- myplot + labs(x = "", y = "")
  }
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    myplot <- myplot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(myplot)
}

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
  
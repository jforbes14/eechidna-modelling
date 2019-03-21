# Visualising estimated models

# Need to construct plots instead of using visreg

library(gridExtra)
library(grid)

# ------------------------------------------------------------------------------------

# Function to get partial residuals
partial_resids <- function(my_model, varname) {
  # Residuals 
  resids <- residuals(my_model)
  
  # Covariate of interest
  if (is.null(my_model$model)) {
    # Spatial
    x_vec <- my_model$X[, varname]
  } else {
    # Linear model
    x_vec <- my_model$model[varname]
  }
  
  # Data
  dat <- data.frame(
    varname = x_vec, 
    part_resids = resids + (x_vec)*my_model$coefficients[varname]
    )
  
  # Rename
  names(dat) <- c("varname", "part_resids")
  
  return(dat)
}


# ------------------------------------------------------------------------------------

# Standardized main effect plot

my_model <- fmod10
sp_weights <- sp_weights_10
varname <- "Unemployment"

std_main_effect_plot <- function(my_model, sp_weights, varname, xlimits = NULL, ylimits = NULL) {
  # Set up
  x_df <- as.data.frame(my_model$X[,-1])
  y_vec <- my_model$y
  coef_df <- my_model$coefficients[-1]
  my_res <- my_model$residuals
  rho <- my_model$lambda
  wmat <- listw2mat(sp_weights)
  scale_mat <- (diag(nrow(wmat)) - rho*wmat)
  
  ## Test
  #x_vec <- my_model$X[, varname]
  #my_coef <- my_model$coefficients[varname]
  #x_star <- scale_mat%*%x_vec
  #res_star <- scale_mat%*%my_res + my_coef*x_star
  #ggplot(aes(x = x_star, y = res_star), data = NULL) + geom_point() + geom_smooth()
  
  # Create whitened objects
  y_star <- scale_mat%*%y_vec
  x_star <- scale_mat%*%as.matrix(x_df)
  data_star <- data.frame(cbind(y_star, x_star))
  names(data_star) <- c("y", names(x_df))
  
  # Model
  lm_star <- lm(y ~ ., data_star)
  
  # Year
  year <- paste0("20", substr(as.character(my_model$call)[4], nchar(as.character(my_model$call)[4]) - 1, nchar(as.character(my_model$call)[4])))
  
  # Visreg
  plot <- visreg(lm_star, varname, gg = T, print.cond = FALSE) + 
    labs(x = "", y = "", title = year) +
    theme_bw() + 
    theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm")) + coord_cartesian(expand = FALSE)

  if (!is.null(xlimits) & !is.null(ylimits)) {
    plot <- plot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }

  return(plot)
}

p1 <- std_main_effect_plot(fmod16, sp_weights_16, "Incomes")
p2 <- main_effect_plot(fmod16, "Incomes")
grid.arrange(p1, p2)

# ------------------------------------------------------------------------------------

# Main effects - standardized partial residuals

# Unemployment
Unemployment_16 <- std_main_effect_plot(fmod16, sp_weights_16, "Unemployment") + labs(title = 2016)
Unemployment_13 <- std_main_effect_plot(fmod13, sp_weights_13, "Unemployment") + labs(title = 2013)
Unemployment_10 <- std_main_effect_plot(fmod10, sp_weights_10, "Unemployment") + labs(title = 2010)
Unemployment_07 <- std_main_effect_plot(fmod07, sp_weights_07, "Unemployment") + labs(title = 2007)
Unemployment_04 <- std_main_effect_plot(fmod04, sp_weights_04, "Unemployment") + labs(title = 2004)
Unemployment_01 <- std_main_effect_plot(fmod01, sp_weights_01, "Unemployment") + labs(title = 2001)

grid.arrange(Unemployment_01, Unemployment_04, Unemployment_07, Unemployment_10, Unemployment_13, Unemployment_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Unemployment", gp = gpar(cex = 0.8)))







# ------------------------------------------------------------------------------------

# Main effect plot

main_effect_plot <- function(my_model, varname, xlimits = NULL, ylimits = NULL) {
  dat <- partial_resids(my_model, varname)
  
  plot <- ggplot(data = dat, aes(x = varname, y = part_resids)) +
    geom_point(alpha = 0.7) + 
    geom_smooth(method = "lm") +
    theme_light() +
    theme(axis.title = element_blank(), plot.title = element_text(face = "bold", size = 10, hjust = 0.5))
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    plot <- plot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(plot)
}


# ------------------------------------------------------------------------------------

# Main effects

# Born UK
Born_UK_16 <- main_effect_plot(mod16, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2016)
Born_UK_13 <- main_effect_plot(mod13, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2013)
Born_UK_10 <- main_effect_plot(mod10, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2010)
Born_UK_07 <- main_effect_plot(mod07, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2007)
Born_UK_04 <- main_effect_plot(mod04, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2004)
Born_UK_01 <- main_effect_plot(mod01, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15)) + labs(title = 2001)

grid.arrange(Born_UK_01, Born_UK_04, Born_UK_07, Born_UK_10, Born_UK_13, Born_UK_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Population born in the United Kingdom", gp = gpar(cex = 0.8)))

# Married
Married_16 <- main_effect_plot(mod16, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2016)
Married_13 <- main_effect_plot(mod13, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2013)
Married_10 <- main_effect_plot(mod10, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2010)
Married_07 <- main_effect_plot(mod07, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2007)
Married_04 <- main_effect_plot(mod04, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2004)
Married_01 <- main_effect_plot(mod01, "Married", ylimits = c(-13,15), xlimits = c(-4.7, 2.5)) + labs(title = 2001)

grid.arrange(Married_01, Married_04, Married_07, Married_10, Married_13, Married_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Married", gp = gpar(cex = 0.8)))

# Unemployment
Unemployment_16 <- main_effect_plot(mod16, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2016)
Unemployment_13 <- main_effect_plot(mod13, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2013)
Unemployment_10 <- main_effect_plot(mod10, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2010)
Unemployment_07 <- main_effect_plot(mod07, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2007)
Unemployment_04 <- main_effect_plot(mod04, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2004)
Unemployment_01 <- main_effect_plot(mod01, "Unemployment", ylimits = c(-15,15), xlimits = c(-2.16, 3.43)) + labs(title = 2001)

grid.arrange(Unemployment_01, Unemployment_04, Unemployment_07, Unemployment_10, Unemployment_13, Unemployment_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Unemployment", gp = gpar(cex = 0.8)))

# Transformative
Transformative_16 <- main_effect_plot(mod16, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2016)
Transformative_13 <- main_effect_plot(mod13, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2013)
Transformative_10 <- main_effect_plot(mod10, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2010)
Transformative_07 <- main_effect_plot(mod07, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2007)
Transformative_04 <- main_effect_plot(mod04, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2004)
Transformative_01 <- main_effect_plot(mod01, "Transformative", ylimits = c(-15,15), xlimits = c(-2.41, 3.03)) + labs(title = 2001)

grid.arrange(Transformative_01, Transformative_04, Transformative_07, Transformative_10, Transformative_13, Transformative_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Transformative", gp = gpar(cex = 0.8)))

# Born_SE_Europe
Born_SE_Europe_16 <- main_effect_plot(mod16, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2016)
Born_SE_Europe_13 <- main_effect_plot(mod13, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2013)
Born_SE_Europe_10 <- main_effect_plot(mod10, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2010)
Born_SE_Europe_07 <- main_effect_plot(mod07, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2007)
Born_SE_Europe_04 <- main_effect_plot(mod04, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2004)
Born_SE_Europe_01 <- main_effect_plot(mod01, "Born_SE_Europe", ylimits = c(-15,16), xlimits = c(-0.95, 5.15)) + labs(title = 2001)

grid.arrange(Born_SE_Europe_01, Born_SE_Europe_04, Born_SE_Europe_07, Born_SE_Europe_10, Born_SE_Europe_13, Born_SE_Europe_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Population born in South-Eastern Europe", gp = gpar(cex = 0.8)))

# Different Address
DiffAddress_16 <- main_effect_plot(mod16, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2016)
DiffAddress_13 <- main_effect_plot(mod13, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2013)
DiffAddress_10 <- main_effect_plot(mod10, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2010)
DiffAddress_07 <- main_effect_plot(mod07, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2007)
DiffAddress_04 <- main_effect_plot(mod04, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2004)
DiffAddress_01 <- main_effect_plot(mod01, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 4)) + labs(title = 2001)

grid.arrange(DiffAddress_01, DiffAddress_04, DiffAddress_07, DiffAddress_10, DiffAddress_13, DiffAddress_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Different Address", gp = gpar(cex = 0.8)))

# ------------------------------------------------------------------------------------

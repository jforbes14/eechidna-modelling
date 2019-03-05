# Visualising estimated models

# Need to construct plots instead of using visreg

# ------------------------------------------------------------------------------------

# Main effects
library(gridExtra)

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

main_effect_plot <- function(my_model, varname, xlimits = NULL, ylimits = NULL) {
  dat <- partial_resids(my_model, varname)
  
  plot <- ggplot(data = dat, aes(x = varname, y = part_resids)) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme(axis.title = element_blank()) 
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    plot <- plot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(plot)
}

# Born UK
Born_UK_16 <- main_effect_plot(mod16, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))
Born_UK_13 <- main_effect_plot(mod13, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))
Born_UK_10 <- main_effect_plot(mod10, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))
Born_UK_07 <- main_effect_plot(mod07, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))
Born_UK_04 <- main_effect_plot(mod04, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))
Born_UK_01 <- main_effect_plot(mod01, "Born_UK", xlimits = c(-1.4, 5), ylimits = c(-13, 15))

grid.arrange(Born_UK_01, Born_UK_04, Born_UK_07, Born_UK_10, Born_UK_13, Born_UK_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Population born in the United Kingdom", gp = gpar(cex = 0.8)))

# Median Age
MedianAge_16 <- main_effect_plot(mod16, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
MedianAge_13 <- main_effect_plot(mod13, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
MedianAge_10 <- main_effect_plot(mod10, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
MedianAge_07 <- main_effect_plot(mod07, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
MedianAge_04 <- main_effect_plot(mod04, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
MedianAge_01 <- main_effect_plot(mod01, "MedianAge", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))

grid.arrange(MedianAge_01, MedianAge_04, MedianAge_07, MedianAge_10, MedianAge_13, MedianAge_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Median Age", gp = gpar(cex = 0.8)))

# ------------------------------------------------------------------------------------

# Interactions



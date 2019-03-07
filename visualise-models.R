# Visualising estimated models

# Need to construct plots instead of using visreg

# ------------------------------------------------------------------------------------

library(gridExtra)
library(grid)

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

# ------------------------------------------------------------------------------------

# Main effects

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

# Married
Married_16 <- main_effect_plot(mod16, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
Married_13 <- main_effect_plot(mod13, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
Married_10 <- main_effect_plot(mod10, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
Married_07 <- main_effect_plot(mod07, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
Married_04 <- main_effect_plot(mod04, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
Married_01 <- main_effect_plot(mod01, "Married", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))

grid.arrange(Married_01, Married_04, Married_07, Married_10, Married_13, Married_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Married", gp = gpar(cex = 0.8)))

# Different Address
DiffAddress_16 <- main_effect_plot(mod16, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
DiffAddress_13 <- main_effect_plot(mod13, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
DiffAddress_10 <- main_effect_plot(mod10, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
DiffAddress_07 <- main_effect_plot(mod07, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
DiffAddress_04 <- main_effect_plot(mod04, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))
DiffAddress_01 <- main_effect_plot(mod01, "DiffAddress", ylimits = c(-15,20), xlimits = c(-2.5, 2.8))

grid.arrange(DiffAddress_01, DiffAddress_04, DiffAddress_07, DiffAddress_10, DiffAddress_13, DiffAddress_16, 
  nrow = 2, 
  left = textGrob("Two-party preferred vote (%)", gp = gpar(cex = 0.8), rot = 90), 
  bottom = textGrob("Different Address", gp = gpar(cex = 0.8)))

# ------------------------------------------------------------------------------------

# Interactions

# Perhaps adapt function to take varnames

my_model <- mod16
varnames <- c("OtherLanguageHome", "Extractive")

# Function to get partial residuals
partial_resids_2 <- function(my_model, varnames) {
  
  # Residuals 
  resids <- residuals(my_model)
  
  # Covariate of interest
  if (is.null(my_model$model)) {
    # Spatial
    
    # Add interaction to varnames
    if (length(varnames) > 1) {
      varnames <- c(varnames, paste0(sort(varnames)[1], ":", sort(varnames)[2]))
    }
    
    # Get covariates
    x_vec <- my_model$X[, varnames]
    
  } else {
    # Linear model
    x_vec <- my_model$model[, varnames]
  }
  
  # Data
  dat <- data.frame(
    my_varnames = x_vec, 
    part_resids = resids + as.matrix(x_vec)%*%my_model$coefficients[varnames]
  )
  
  # Rename
  if (ncol(dat) > 2) {
    names(dat)[1:2] <- c("varname1", "varname2")
  } else {
    names(dat) <- c("varname", "part_resids")
  }
  
  return(dat)
}


# Interaction plot

interaction_plot <- function(my_model, varnames, year) {
  
  # Get data from model into data frame
  if (is.null(my_model$model)) {
    # Spatial
    X_names <- unlist(dimnames(my_model$X)[2])
    all_varnames <- X_names[-grep("(Intercept)", X_names)]
    #all_varnames <- X_names[-c(grep("(Intercept)", X_names), grep(":", X_names))]
    df <- data.frame(my_model$X[, all_varnames])
  } else {
    # Linear model
    df <- data.frame(my_model$model[, -1])
  }
  
  # Get min and max of each variable in interaction
  my_min <- floor(min(min(df[, varnames[1]]), min(df[, varnames[2]])))
  my_max <- ceiling(max(max(df[, varnames[1]]), max(df[, varnames[2]])))
  
  # Range of min max
  my_range = my_max - my_min
  num_rows = length(rep(seq(my_min, my_max, by = 0.01), times = (my_range*100 + 1)))
  
  # Skeleton 2D heatmap
  plot_df <- data.frame(matrix(0, nrow = num_rows, ncol = ncol(df)))
  names(plot_df) <- names(df)
  plot_df[, varnames[1]] <- rep(seq(my_min, my_max, by = 0.01), times = (my_range*100 + 1))
  plot_df[, varnames[2]] <- rep(seq(my_min, my_max, by = 0.01), each = (my_range*100 + 1))
  plot_df[, paste0(sort(varnames)[1],".",sort(varnames)[2])] <- plot_df[, varnames[1]]*plot_df[, varnames[2]]
  
  # Add predicted values to show effect
  if (is.null(my_model$model)) {
    # Spatial
    plot_df$z <- as.matrix(plot_df)%*%as.matrix(my_model$coefficients[-1])
  } else {
    # Linear model
    plot_df$z <- predict(my_model, plot_df) - my_model$coefficients[1]
  } 
  
  # Rename variables for plotting
  names(plot_df)[which(names(plot_df) == varnames[1])] <- "var1"
  names(plot_df)[which(names(plot_df) == varnames[2])] <- "var2"
  
  # Get electorate points with partial residuals
  part_resids <- partial_resids_2(my_model, varnames)
  
  # Construct ggplot
  my_plot <- plot_df %>% ggplot(aes(x=var1, y=var2)) +
    geom_raster(aes(fill = z)) + 
    labs(x = "", y = "", fill = "Marginal effect on two-party preferred (%)", title = as.character(year)) +
    theme_bw() + 
    theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5)) + 
    scale_fill_gradientn(
      colours = c("#ff0000", "#ff6666", "#ff9999", "#ffcccc", "white", 
        "#cce6ff", "#3399ff", "#3385ff", "#3366ff"), 
      values = scales::rescale(c(min(plot_df$z), -12, -6, -3, 0, 3, 6, 12, max(plot_df$z)))) +  
   geom_point(aes(x=varname1, y=varname2, col=part_resids), alpha = 0.7, data = part_resids, size = 0.8) +  
    geom_point(aes(x=varname1, y=varname2), shape = 1, data = part_resids, alpha = 0.3, size = 0.8) +  
    scale_color_gradientn(colours = c("#ff0000", "#ff6666", "#ff9999", "#ffcccc", "white", "#cce6ff", "#3399ff", "#3385ff", "#3366ff"), 
      values = scales::rescale(c(min(part_resids$part_resids), -8, -4, -2, 
        0, 2, 4, 8, max(part_resids$part_resids)))) +  
    guides(fill = FALSE, col = FALSE) +
    lims(x = c(min(df[,varnames[1]]) - 0.25, max(df[,varnames[1]]) + 0.25), y = c(min(df[,varnames[2]]) - 0.25, max(df[,varnames[2]]) + 0.25))
  
  return(my_plot)
}

# Test
varnames <- c("OtherLanguageHome", "Extractive")
my_model <- mod16
year = 2016
interaction_plot(mod16_lm, c("Unemployment", "Extractive"), 2016)


mod16_lm <- lm(formula = LNP_Percent ~ . + 
    OtherLanguageHome:Extractive + OtherLanguageHome:DeFacto + 
    ManagerAdminClericalSales:Extractive + OtherLanguageHome:Born_SE_Europe + Unemployment:Extractive, 
  data = (small_df %>% filter(year == "2016") %>% dplyr::select(c(LNP_Percent, superset_vars))))
my_model <- mod16_lm
interaction_plot(mod16_lm, c("Unemployment", "Extractive"), 2016)



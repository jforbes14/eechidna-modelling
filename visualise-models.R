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

interaction_plot <- function(my_model, varnames, year = "Enter Year", xlimits = NULL, ylimits = NULL) {
  
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
  # Add a 20% buffer on the outside
  my_min <- floor(min(min(df[, varnames[1]]), min(df[, varnames[2]])))*1.5
  my_max <- ceiling(max(max(df[, varnames[1]]), max(df[, varnames[2]])))*1.5
  
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
    guides(fill = FALSE, col = FALSE)
  
  if (!is.null(xlimits) & !is.null(ylimits)) {
    my_plot <- my_plot + coord_cartesian(xlim = xlimits, ylim = ylimits)
  }
  
  return(my_plot)
}

# ------------------------------------------------------------------------------------

# Plot all interactions
library(png)
TPP_scale <- rasterGrob(readPNG("figures/TPP_scale.png"))

# Extractive:OtherLanguageHome
Extract_Other_16 <- interaction_plot(mod16, c("Extractive", "OtherLanguageHome"), 2016,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
Extract_Other_13 <- interaction_plot(mod13, c("Extractive", "OtherLanguageHome"), 2013,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
Extract_Other_10 <- interaction_plot(mod10, c("Extractive", "OtherLanguageHome"), 2010,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
Extract_Other_07 <- interaction_plot(mod07, c("Extractive", "OtherLanguageHome"), 2007,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
Extract_Other_04 <- interaction_plot(mod04, c("Extractive", "OtherLanguageHome"), 2004,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
Extract_Other_01 <- interaction_plot(mod01, c("Extractive", "OtherLanguageHome"), 2001,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))

grid.arrange(Extract_Other_01, Extract_Other_04, Extract_Other_07, Extract_Other_10, Extract_Other_13, Extract_Other_16, TPP_scale,
  nrow = 3, 
  widths = c(1,1,1),
  heights = c(0.25, 1, 1),
  layout_matrix = rbind(c(7, 7, 7),
    c(1,2,3),
    c(4,5,6)),
  bottom = textGrob("Extractive", gp = gpar(cex = 0.8)), 
  left = textGrob("Other Language Home", gp = gpar(cex = 0.8), rot = 90))

# DeFacto:OtherLanguageHome
DeFacto_Other_16 <- interaction_plot(mod16, c("DeFacto", "OtherLanguageHome"), 2016,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
DeFacto_Other_13 <- interaction_plot(mod13, c("DeFacto", "OtherLanguageHome"), 2013,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
DeFacto_Other_10 <- interaction_plot(mod10, c("DeFacto", "OtherLanguageHome"), 2010,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
DeFacto_Other_07 <- interaction_plot(mod07, c("DeFacto", "OtherLanguageHome"), 2007,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
DeFacto_Other_04 <- interaction_plot(mod04, c("DeFacto", "OtherLanguageHome"), 2004,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))
DeFacto_Other_01 <- interaction_plot(mod01, c("DeFacto", "OtherLanguageHome"), 2001,
  xlimits = c(min(small_df$DeFacto) - 0.15, max(small_df$DeFacto) + 0.15), 
  ylimits = c(min(small_df$OtherLanguageHome) - 0.15, max(small_df$OtherLanguageHome) + 0.15))

grid.arrange(DeFacto_Other_01, DeFacto_Other_04, DeFacto_Other_07, DeFacto_Other_10, DeFacto_Other_13, DeFacto_Other_16, TPP_scale,
  nrow = 3, 
  widths = c(1,1,1),
  heights = c(0.25, 1, 1),
  layout_matrix = rbind(c(7, 7, 7),
    c(1,2,3),
    c(4,5,6)),
  bottom = textGrob("DeFacto", gp = gpar(cex = 0.8)), 
  left = textGrob("Other Language Home", gp = gpar(cex = 0.8), rot = 90))

# Extractive:ManagerAdminClericalSales
Extract_Admin_16 <- interaction_plot(mod16, c("Extractive", "ManagerAdminClericalSales"), 2016,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))
Extract_Admin_13 <- interaction_plot(mod13, c("Extractive", "ManagerAdminClericalSales"), 2013,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))
Extract_Admin_10 <- interaction_plot(mod10, c("Extractive", "ManagerAdminClericalSales"), 2010,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))
Extract_Admin_07 <- interaction_plot(mod07, c("Extractive", "ManagerAdminClericalSales"), 2007,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))
Extract_Admin_04 <- interaction_plot(mod04, c("Extractive", "ManagerAdminClericalSales"), 2004,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))
Extract_Admin_01 <- interaction_plot(mod01, c("Extractive", "ManagerAdminClericalSales"), 2001,
  xlimits = c(min(small_df$Extractive) - 0.15, max(small_df$Extractive) + 0.15), 
  ylimits = c(min(small_df$ManagerAdminClericalSales) - 0.15, max(small_df$ManagerAdminClericalSales) + 0.15))

grid.arrange(Extract_Admin_01, Extract_Admin_04, Extract_Admin_07, Extract_Admin_10, Extract_Admin_13, Extract_Admin_16, TPP_scale,
  nrow = 3, 
  widths = c(1,1,1),
  heights = c(0.25, 1, 1),
  layout_matrix = rbind(c(7, 7, 7),
    c(1,2,3),
    c(4,5,6)),
  bottom = textGrob("Extractive", gp = gpar(cex = 0.8)), 
  left = textGrob("Manager Admin Clerical Sales", gp = gpar(cex = 0.8), rot = 90))

# MedianAge:OneParent_House
Age_OP_16 <- interaction_plot(mod16, c("MedianAge", "OneParent_House"), 2016,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))
Age_OP_13 <- interaction_plot(mod13, c("MedianAge", "OneParent_House"), 2013,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))
Age_OP_10 <- interaction_plot(mod10, c("MedianAge", "OneParent_House"), 2010,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))
Age_OP_07 <- interaction_plot(mod07, c("MedianAge", "OneParent_House"), 2007,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))
Age_OP_04 <- interaction_plot(mod04, c("MedianAge", "OneParent_House"), 2004,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))
Age_OP_01 <- interaction_plot(mod01, c("MedianAge", "OneParent_House"), 2001,
  xlimits = c(min(small_df$MedianAge) - 0.15, max(small_df$MedianAge) + 0.15), 
  ylimits = c(min(small_df$OneParent_House) - 0.15, max(small_df$OneParent_House) + 0.15))

grid.arrange(Age_OP_01, Age_OP_04, Age_OP_07, Age_OP_10, Age_OP_13, Age_OP_16, TPP_scale,
  nrow = 3, 
  widths = c(1,1,1),
  heights = c(0.25, 1, 1),
  layout_matrix = rbind(c(7, 7, 7),
    c(1,2,3),
    c(4,5,6)),
  bottom = textGrob("Median Age", gp = gpar(cex = 0.8)), 
  left = textGrob("One Parent Households", gp = gpar(cex = 0.8), rot = 90))
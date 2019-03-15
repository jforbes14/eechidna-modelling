# Functions

# ---------------------------------------------------------------------------------

# Orient PCs
orientPCs <- function(pca) {
  
  df <- pca$rotation %>% as.data.frame() %>% rownames_to_column(var = "Variable")
  
  PC1 = pca$rotation[order(names(pca$rotation[,1])),1]
  if (PC1[which(names(PC1) == "HighSchool")] < 0) {
    PC1 = -PC1
  }
  
  PC2 = pca$rotation[order(names(pca$rotation[,1])),2]
  if (PC2[which(names(PC2) == "FamilyRatio")] < 0) {
    PC2 = -PC2
  }
  
  PC3 = pca$rotation[order(names(pca$rotation[,1])),3]
  if (PC3[which(names(PC3) == "Unemployed")] < 0) {
    PC3 = -PC3
  }
  
  PC4 = pca$rotation[order(names(pca$rotation[,1])),4]
  if (PC4[which(names(PC4) == "Owned")] < 0) {
    PC4 = -PC4
  }
  
  df[, 2:5] <- cbind(PC1, PC2, PC3, PC4)
  
  return(df)
}

# ---------------------------------------------------------------------------------

# Standardize variables
standardise_vars <- function(df) {
  hold <- df
  num_cols <- sapply(df, class) == 'numeric'
  df[, num_cols] <- lapply(df[, num_cols], scale)
  names(df) <- names(hold)
  df$LNP_Percent <- hold$LNP_Percent
  return(df)
}

# ---------------------------------------------------------------------------------

# All X way linear models

all_X_way <- function(y_vec, x_df, n_vars) {
  
  index <- combn(x = c(1:ncol(x_df)), m = n_vars)
  
  Xway_models <- data.frame(iter = c(1:ncol(index))) %>%  
    mutate(intercept = 0, r2 = 0, adj_r2 = 0, AIC = 0, BIC = 0, logL = 0)
  
  for (i in 1:ncol(index)) {
    
    # Combine the data
    mat_data <- cbind(x_df, y_vec)
    
    #a <- index[,i][1] 
    #b <- index[,i][2]
    #c <- index[,i][3]
    #d <- index[,i][4]
    #e <- index[,i][5]
    
    # Fit regression
    fit <- lm(y_vec ~ ., mat_data[, index[,i]])
    
    # Model statistics
    sumr = summary(fit)
    aic = AIC(fit)
    bic = BIC(fit)
    logL = logLik(fit)
    r2 = sumr$r.squared
    adj_r2 = sumr$adj.r.squared
    
    Xway_models$r2[i] = r2
    Xway_models$adj_r2[i] = adj_r2
    Xway_models$AIC[i] = aic
    Xway_models$BIC[i] = bic
    Xway_models$logL[i] = logL
    
    # Variables, coefficients and t statistics
    Xway_models$intercept[i] = fit$coefficients[1]
    
    Xway_models$vars[i] = list(index[,i])
    
    #Xway_models$varnames[i] = list(names(x_df)[index[,i]])
    
    Xway_models$coef[i] = list(fit$coefficients[-1] %>% unname)
    
    Xway_models$t[i] = list(sumr$coefficients[-1,"t value"] %>% unname)
    
    
    if (i %% 5000 == 0) {
      print(paste("rep",i))
    }
    
  }
  
  return(Xway_models)
  
}

# ---------------------------------------------------------------------------------

# Compute variable importance function using Akaike Weights

var_imp <- function(Xway, x_df) {
  
  # Best global AIC
  best_AIC <- min(Xway$AIC)
  
  # Akaike weights
  w_vars <- Xway %>% 
    mutate(delta = AIC - best_AIC,
      w_numer = exp(-delta/2),
      w_denom = sum(w_numer),
      w = w_numer/w_denom) %>% 
    dplyr::select(vars, w)
  
  # Inferring number of columns in X
  n_col_x = Xway %>% filter(iter %in% 1:100) %>% dplyr::select(vars) %>% unlist %>% max
  
  # Empty variable importance df
  var_imp <- data.frame(var = 1:n_col_x, sum_w = 0, coef_wsum = 0)
  
  for (i in 1:nrow(w_vars)) {
    # Variables
    vars <- w_vars$vars[i] %>% unlist %>% unname
    
    # Add weight to sum of weights
    w <- w_vars$w[i]
    var_imp[vars, "sum_w"] <- var_imp[vars, "sum_w"] + w
    
    # Add running tally of weighted coefficient
    var_imp[vars, "coef_wsum"] <- var_imp[vars, "coef_wsum"] + w*(Xway[i, ]$coef %>% unlist)
  }
  
  # Compute weighted model average of coeffficient
  var_imp <- var_imp %>% 
    mutate(coef_wavg = (coef_wsum / sum_w))
  
  # Attach variable names
  names <- data.frame(var = 1:ncol(x_df), varname = names(x_df))
  
  var_imp <- var_imp %>% 
    left_join(names, by = "var")
  
  return(var_imp)
}





# ---------------------------------------------------------------------------------

# Compute spatial weights matrix

sp_weights_matrix <- function(sF) {

dist_matrix <- function(shapefile) {
  dist_mat <- matrix(NA, nrow = 150, ncol = 150)
  rownames(dist_mat) <- sort(shapefile$elect_div)
  colnames(dist_mat) <- sort(shapefile$elect_div)
  
  for (i in 1:(nrow(dist_mat)-1)) {
    rname = rownames(dist_mat)[i]
    row_poly = shapefile %>% subset(elect_div == rname)
    
    for (j in (i+1):ncol(dist_mat)) {
      
      cname = rownames(dist_mat)[j]
      col_poly = shapefile %>% subset(elect_div == cname)
      dist = gDistance(row_poly, col_poly)
      dist_mat[i,j] = dist
      
    }
    print(i)
  }
  
  # Now copy to lower triange
  for (i in 2:nrow(dist_mat)) {
    for (j in 1:(i-1)) {
      dist_mat[i,j] = dist_mat[j,i]
    }
  }
  
  # Check it is symmetric
  if(!isSymmetric(dist_mat)) {
    print("Warning! Matrix is not symmetric. Error has occured.")
  }
  
  return(dist_mat)
}

my_dist_mat <- dist_matrix(sF)

# S matrix

s_mat <- function(dist_mat) {
  s_mat <- dist_mat
  
  for (i in 1:nrow(dist_mat)) {
    for (j in 1:nrow(dist_mat)) {
      a = dist_mat[i,j]
      
      if (is.na(a)) {
        b = 0
      } else {
        b = ifelse(a == 0, 1, 0)
      }
      
      s_mat[i,j] = b
      
    }
  }
  
  return(s_mat)
}

my_smat <- s_mat(my_dist_mat)

# Turn into W matrix

w_mat <- function(s_mat) {
  
  w_mat <- s_mat
  rsums = rowSums(s_mat)
  
  for (i in 1:nrow(s_mat)) {
    w_mat[i,] <- s_mat[i,]/rsums[i]
  }
  
  return(w_mat)
}

my_wmat <- w_mat(my_smat)

# Turn into listw

my_listw <- mat2listw(my_wmat)

return(my_listw)

}


# ---------------------------------------------------------------------------------


# All X way spatial error models

all_sperr_X_way <- function(y_vec, x_df, sp_weights, n_vars) {
  
  index <- combn(x = c(1:ncol(x_df)), m = n_vars)
  
  Xway_models <- data.frame(iter = c(1:ncol(index))) %>%  
    mutate(intercept = 0, corr_coef = 0, AIC = 0, AIC_lm =0, BIC = 0, logL = 0, logL_lm = 0)
  
  for (i in 1:ncol(index)) {
    
    # Combine the data
    mat_data <- cbind(x_df, y_vec)

    # Fit spatial error model
    fit <- errorsarlm(y_vec ~ ., data=mat_data[,c(index[,i],ncol(mat_data))], sp_weights, 
      etype="error", method="eigen", interval=c(-1,0.999))
    
    
    # Model statistics
    sumr = summary(fit)
    aic = AIC(fit)
    aic_lm = fit$AIC_lm.model
    bic = BIC(fit)
    logL = logLik(fit)
    logL_lm = fit$logLik_lm.model

    Xway_models$AIC[i] = aic
    Xway_models$BIC[i] = bic
    Xway_models$logL[i] = logL
    
    # Variables, coefficients and t statistics
    Xway_models$intercept[i] = fit$coefficients[1]
    
    Xway_models$corr_coef[i] = fit$lambda
    
    Xway_models$vars[i] = list(index[,i])
    
    Xway_models$coef[i] = list(fit$coefficients[-1] %>% unname)
    
    Xway_models$coef_lm[i] = list(fit$coef_lm.model[-1] %>% unname)
    
    Xway_models$se[i] = list(fit$rest.se[-1] %>% unname)
    
    
    if (i %% 10 == 0) {
      print(paste("rep",i))
    }
    
  }
  
  return(Xway_models)
  
}

# ---------------------------------------------------------------------------------

# Make more general age baskets: 0-24, 25-54, 55-onwards
basket_age <- function(abs_df) {
  abs_df <- abs_df %>% 
    mutate(Age00_19 = Age00_04 + Age05_14 + Age15_19, 
      Age20_34 = Age20_24 + Age25_34,
      Age35_54 = Age35_44 + Age45_54, 
      Age55plus = Age55_64 + Age65_74 + Age75_84 + Age85plus) %>% 
    select(-c(Age00_04, Age05_14, Age15_19, Age20_24, Age25_34, 
      Age35_44, Age45_54, Age55_64, Age65_74, Age75_84, Age85plus, MedianAge))
  return(abs_df)
}

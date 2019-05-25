CV_gam <- function(form, dat, resp_name = "y", K = 10) {
  # form: the gam formula
  # dat: the data set containing the response variable and the covariates
  # resp_name: the name of the response variable
  # K: the number of folds
  
  # manual set up
  #form <- form
  #dat <- train_tr
  #resp_name <- "y"
  #K <- 10
  
  # packages
  require("tidyverse")
  require("mgcv")
  
  # Basis info
  n <- nrow(dat)
  
  # Decompose the data into K folds ----
  gr <- as.factor(1:K)  # create the groups
  dat_shuf <- sample_n(dat, n)  # shuffle the data
  folds <- split(dat_shuf, gr)
  
  # Create a vector of MSE
  MSE_k <- rep(0, K)
  RSS_k <- rep(0, K)
  
  # For each fold ----
  for (k in 1:K) {
    
    # Create training and test set
    cur_test <- folds[[k]]
    cur_training <- bind_rows(folds[-k])
    
    # Fit a model on the training set
    cur_mod <- gam(form, data = cur_training)
    
    # Compute MSE_k
    MSE_k[k] <- (K / n) * sum(
      (cur_test[, resp_name] - predict(cur_mod, newdata = cur_test)) ^ 2)
    
  }
  
  # Compute the CV estimator ----
  CV_K <- mean(MSE_k)
 
  # Compute the SE of the CV estimator ----
  se_CV <- (1 / sqrt(K)) * sqrt(sum(
    (MSE_k - CV_K)^2 / (K - 1)
  ))
  
  # Take sqrt the MSE
  RMSE_CV <- CV_K %>% sqrt()
  
  # Return the CV error
  return(RMSE_CV)
}
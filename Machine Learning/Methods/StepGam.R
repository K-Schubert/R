# StepGam

# The aim of this algorithm is to suggest which continuous variable could be 
# added in a GAM model in order to improve the CV-error. 
#
# Arguments:
#   - base_model: the basis model used
#   - data: the data set
#   - outcome: the name of the outcome variable
#
# It works as follows:
#
# StepGam <- function(base_model, data, outcome = "y")
#   Create a table with (1) model (2) new variable (3) RMSE (4) CV-error
#   fit the model
#   compute RMSE
#   compute CV-error
#   Create list of predictors to use
#   for each potential new predictor
#     add it to the model and fit
#     compute new RMSE
#     compute new CV-error
#     Put errors in table
#   determine best variable to add considering cv-error
#   print best variable to add considering cv-error
#   Return table
#
StepGam <- function(base_model, data, outcome = "y") {
  # Trial values
  #base_model <- gam_basic
  #data <- train_tr
  #outcome <- "y"
  
  # Libraries
  require("tidyverse")
  require("caret")
  
  # Setup
  n <- nrow(data)
  p <- ncol(data) - 1
  form <- base_model$formula
  
  # Extract the list of predictors used
  pred_used <- base_model$pred.formula %>% as.character()
  pred_used <- pred_used[2]
  pred_used <- str_split(pred_used, pattern = " \\+ ", simplify = TRUE)
  pred_used <- pred_used[1, ]
  
  # Determine which other predictors to try
  further_preds_cl <- data %>% select(-outcome) %>% lapply(class) %>% unlist()
  
  # Remove those already used
  further_preds <- further_preds_cl[!(names(further_preds_cl) %in% pred_used)]
  
  # Remove factors
  further_preds <- further_preds[!(further_preds %in% "factor")]
  further_preds <- names(further_preds)
  
  # inspect result
  #further_preds
  
  # Create summary table
  tbl_l <- length(further_preds) + 1
  tbl <- tibble(model = character(length = tbl_l),
                newvar = character(length = tbl_l), 
                rmse = numeric(length = tbl_l),
                cv_error = numeric(length = tbl_l))
  
  # Fill table for the first time
  tbl[1, 1] <- paste(pred_used, collapse = " + ")
  tbl[1, 2] <- "none"
  tbl[1, 3] <- caret::RMSE(data[,outcome] %>% unlist(), predict(base_model))
  tbl[1, 4] <- CV_gam(form = form, dat = data, resp_name = outcome, K = 10)

  # Try to add continuous predictors
  for (i in 1:length(further_preds)) {
    # Fit a new model
    cur_pred <- further_preds[i]
    eq_new <- paste(outcome, 
                  "~", 
                  paste(as.character(form)[3], collapse = " + "), 
                  "+",
                  "s(",
                  cur_pred, 
                  ")")
    cur_mod <- gam(formula = as.formula(eq_new), data = data)
    
    # Write var and formula infos in table
    tbl[i + 1, 1] <- eq_new
    tbl[i + 1, 2] <- cur_pred
    
    # compute and store RMSE
    cur_RMSE <- caret::RMSE(data[,outcome] %>% unlist(), predict(cur_mod))
    tbl[i + 1, 3] <- cur_RMSE
    
    # compute and store CV-error
    cur_cverror <- CV_gam(form = as.formula(eq_new),
                          dat = data, 
                          resp_name = outcome, 
                          K = 10)
    tbl[i + 1, 4] <- cur_cverror
    
  } 
  
  opt_row <- tbl[which.min(tbl$cv_error),]
  
  # Determine id of each variable
  tbl %<>% mutate(id = newvar %>% str_replace("X", "") %>% as.integer)
  
  # Print optimal variable
  print(
    paste(
      "Consider adding variable", 
      opt_row["newvar"], 
      "in your model. It has a CV-error of",
      opt_row["cv_error"]
      )
  )
  
  # Return table
  return(tbl)
  
}
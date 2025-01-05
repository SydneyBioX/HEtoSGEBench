library(infotheo) # for mutual information
library(pROC) # For AUC calculation

# Function to calculate Mutual Information (MI)
calculate_MI <- function(predicted, actual, bins) {
  # Calculate histogram for predicted and actual values
  hist_pred <- hist(predicted, breaks = bins, plot = FALSE)
  hist_actual <- hist(actual, breaks = bins, plot = FALSE)
  
  # Calculate joint histogram
  joint_hist <- hist2d(predicted, actual, breaks = list(hist_pred$breaks, hist_actual$breaks), plot = FALSE)
  
  # Calculate marginal histograms
  marginal_hist_pred <- row_sums(joint_hist)
  marginal_hist_actual <- col_sums(joint_hist)
  
  # Convert histograms to probabilities
  marginal_prob_pred <- marginal_hist_pred / sum(marginal_hist_pred)
  marginal_prob_actual <- marginal_hist_actual / sum(marginal_hist_actual)
  joint_prob <- joint_hist / sum(joint_hist)
  
  # Calculate Mutual Information (MI)
  mi <- sum(joint_prob * log2(joint_prob / (marginal_prob_pred %*% t(marginal_prob_actual))))
  
  return(mi)
}

# Function to calculate Mutual Information (MI) between predicted and actual values
calculate_MI <- function(predicted, actual, bins) {
  
  # Calculate the number of bins based on the length of predicted values
  # using the formula: max(10, length(predicted)^(1/3))
  bins = max(10, length(predicted)^(1/3))
  
  # Discretize predicted and actual values into equal frequency bins
  # using the 'discretize' function with 'equalfreq' method and 'nbins' argument
  predicted_d <- discretize(predicted, disc="equalfreq", nbins=bins)
  actual_d <- discretize(actual, disc="equalfreq", nbins=bins)
  
  # Calculate Mutual Information (MI) between predicted and actual values
  mi <- mutinformation(predicted_d, actual_d)
  
  # Normalize MI by dividing it by the square root of the product of entropies of
  # predicted and actual values
  norm_mi <- mi / sqrt(infotheo::entropy(predicted_d) * infotheo::entropy(actual_d))
  
  # Return the normalized MI value
  return(norm_mi)
}

# Function to calculate Normalized Root Mean Squared Error (NRMSE) with different options
calculate_nrmse <- function(predicted, actual, method = "range") {
  
  if (method == "range") {
    rmse <- sqrt(mean((actual - predicted)^2)) # Calculate RMSE
    range_actual <- max(actual) - min(actual) # Calculate range of actual values
    nrmse <- rmse / range_actual # Calculate NRMSE using range
  } else if (method == "mean") {
    rmse <- sqrt(mean((actual - predicted)^2)) # Calculate RMSE
    mean_actual <- mean(actual) # Calculate mean of actual values
    nrmse <- rmse / mean_actual # Calculate NRMSE using mean
  } else if (method == "sd") {
    rmse <- sqrt(mean((actual - predicted)^2)) # Calculate RMSE
    sd_actual <- sd(actual) # Calculate standard deviation of actual values
    nrmse <- rmse / sd_actual # Calculate NRMSE using standard deviation
  } else {
    stop("Invalid method argument. Please choose 'range', 'mean', or 'sd'.")
  }
  
  return(nrmse)
}

calculate_JS_divergence <- function(pred, exprs) {
  # Compute the normalized expression values and predicted values
  norm_exprs <- if (sum(exprs) == 0) 1/length(exprs) else exprs/sum(exprs)
  norm_pred <- if (sum(pmax(pred, 0)) == 0) 1/length(pred) else pmax(pred, 0)/sum(pmax(pred, 0))
  
  # Compute the JS divergence between norm_exprs and norm_pred
  js_divergence <- philentropy::distance(x=rbind(norm_exprs,norm_pred), 
                                         method="jensen-shannon", 
                                         unit="log2",
                                         mute.message = TRUE)
  
  return(js_divergence)
}

calculate_SSIM <- function(x, y, numBreaks = 256) {
  x <- c(x)
  y <- c(y)
  
  if (max(x) == 0) {x=rep(0,length(x))} else {x <- x / max(x)}
  if (max(y) == 0) {y=rep(0,length(y))} else {y <- y / max(y)}
  
  x.dig <- cut(as.numeric(x), numBreaks, labels = F) - 1
  y.dig <- cut(as.numeric(y), numBreaks, labels = F) - 1
  rm(x, y)
  
  C1 <- (0.01 * (numBreaks - 1))^2
  C2 <- (0.03 * (numBreaks - 1))^2
  
  mux <- mean(x.dig)
  muy <- mean(y.dig)
  sigxy <- cov(x.dig, y.dig)
  sigx <- var(x.dig)
  sigy <- var(y.dig)
  
  ssim <- ((2 * mux * muy + C1) * (2 * sigxy + C2)) / ((mux**2 + muy**2 + C1) * (sigx + sigy + C2))
  stopifnot(ssim >= -1 && ssim <= 1)
  
  return(ssim)
}

# Function to calculate AUC
calculate_AUC <- function(predicted, actual, cutoff) {
  if (length(unique(actual > 0)) == 1) {
    return(NA)
  } else {
    # Binarise actual values
    response = ifelse(actual > cutoff, 1, 0)
    if (length(unique(response)) == 1) {return(NA)}
    
    # Calculate auc
    auc_val <- pROC::auc(response, predicted, direction="<",
                   quiet = TRUE)
    
    return(as.numeric(auc_val))
  }
}


# mlr3 result functions
getAdjustedPredictions <- function(x) {
  #' x here is a data table with row_id, truth, response, prob.responder, prob.non.responsder
  #' This function adds one column with the new predictions adjusting for cutoff
  isErr <- FALSE
  
  tryCatch({
    x <- x %>%
      mutate(prob.pos = get(paste0("prob.", pos_class_name)))
    cp <- cutpointr(x, prob.pos, truth,
                    direction=">=",
                    pos_class= pos_class_name,
                    method = minimize_metric, metric = roc01)$optimal_cutpoint
    x <- x %>%
      mutate(response_adj = ifelse(prob.pos >= cp,
                                   pos_class_name,
                                   neg_class_name),
             cutoff_adj = cp) %>%
      select(-prob.pos)
  },
  error=function(cond) {
    message(cond)
    isErr <<- TRUE
  })
  if (isErr) {
    x <- x %>%
      mutate(response_adj = response,
             cutoff_adj = NA)
  }
  return(x)
}

getAllPredictions <- function(bmr_aggregate, bmr_dt, adj_cutpoint=TRUE) {
  # Get all Predictions
  all_test_n <- vapply(bmr_dt$prediction, 
                       function(x) { nrow(as.data.table(x)) },
                       1)
  
  all_train_n <- lapply(bmr_aggregate$resample_result, 
                        function(x) { 
                          lapply(x$predictions(predict_sets="train"), function(y) {
                            nrow(as.data.table(y))
                          })
                        }) %>% unlist()
  
  all_pred_df <- bind_rows(lapply(bmr_aggregate$resample_result, # Extract prediction probabilities for test set and calculate AUC
                                  function(x) { 
                                    lapply(x$predictions(predict_sets="test"), function(y) {
                                      as.data.table(y)
                                    }) %>%
                                      bind_rows()
                                  })) %>%
    mutate(task_id = rep(unlist(lapply(bmr_dt$task, function(x) {x$id})), times=all_test_n),
           learner_id = rep(unlist(lapply(bmr_dt$learner, function(x) {x$id})), times=all_test_n),
           test_samp = "test_set",
           cv_loop = 1:(length(bmr_dt$iteration)/
                          n_folds) %>% rep(each=n_folds) %>% rep(times=all_test_n),
           iteration = bmr_dt$iteration %>%rep(times = all_test_n)) %>%
    bind_rows(
      bind_rows(lapply(bmr_aggregate$resample_result, # Extract prediction probabilities for test set and calculate AUC
                       function(x) { 
                         lapply(x$predictions(predict_sets="train"), function(y) {
                           as.data.table(y)
                         }) %>%
                           bind_rows()
                       })) %>%
        mutate(task_id = rep(unlist(lapply(bmr_dt$task, function(x) {x$id})), times=all_train_n),
               learner_id = rep(unlist(lapply(bmr_dt$learner, function(x) {x$id})), times=all_train_n),
               test_samp = "train_set",
               cv_loop = 1:(length(bmr_dt$iteration)/n_folds) %>% rep(each=n_folds) %>% rep(times=all_train_n),
               iteration = bmr_dt$iteration %>%rep(times = all_train_n)) 
    )
  
  if (adj_cutpoint) {
    all_pred_df <- all_pred_df %>%
      group_by(task_id, learner_id, test_samp, cv_loop, iteration) %>%
      group_modify(~getAdjustedPredictions(.x)) %>%
      mutate(correct_bool_adj = truth == response_adj,
             correct_bool = truth == response)
  }
  
  return(all_pred_df)
}
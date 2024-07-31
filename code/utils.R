# Function for calculating AMPD and MBE for a given predictor
calculate_ampd_mbe <- function(predictor, data_test, data_counterfactual, idxs) {
  pred_test <- predictor$predict(data_test[idxs, ])
  pred_cf <- predictor$predict(data_counterfactual[idxs, ])
  
  AMPD <- abs(pred_test[, 1] - pred_cf[, 1])
  MBE <- (sum(AMPD)) / length(AMPD)
  
  return(list(AMPD = AMPD, MBE = MBE))
}


calculate_ampd_mbe_moc <- function(predictor, data_test, idxs) {
  # Set seed for reproducibility
  set.seed(123)
  # Initialize lists to store counterfactuals and predictions
  cf_list <- list()
  pred_list <- list()
  
  # Create an instance of CFClassif
  cf_classif <- CFClassif$new(predictor, protected = "Sex", n_generations = generations)
  
  # Loop through all instances in idxs
  for (i in seq_along(idxs)) {
    # Generate counterfactuals for the current instance
    cfactuals <- cf_classif$find_counterfactuals(
      x_interest = data_test[idxs[i], ], desired_class = "female", desired_prob = c(0.5, 1)
    )
    cfactuals$subset_to_valid()
    
    # Store counterfactuals and predictions
    cf_list[[i]] <- cfactuals
    pred1 <- predictor$predict(cfactuals$x_interest)
    pred2 <- predictor$predict(cfactuals$data)
    pred_list[[i]] <- list(pred1 = pred1, pred2 = pred2)
  }
  
  # Calculate AMPD for all instances
  ampd <- sapply(pred_list, function(preds) {
    abs(sum(preds$pred1[, 1] - preds$pred2[, 1])) / nrow(preds$pred2)
  })
  
  # Calculate MBE
  mbe <- sum(ampd, na.rm = TRUE) / length(which(!is.na(ampd)))
  
  return(list(AMPD = ampd, MBE = mbe))
}

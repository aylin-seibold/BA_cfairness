library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
library("data.table")
library("dplyr")
source("code/sim_data.R")
devtools::load_all("extern/counterfactuals")

#-----------#
# Data without unmeasured confounder
#-----------#


# Random Forest
SEED <- 123
generations = 30L
idxs = which(data_test$Sex == "male")

# prep data
data_train <- data_no_confounder_train # train data
data_test <- data_no_confounder_test # test data
# counterfactual data do(Sex = female)
data_counterfactual <- data_no_confounder_counterfactual_female 


# log reg

learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_train, target = "Risk")
learner$predict_type = "prob"

set.seed(SEED)
learner$train(task)
predictor = iml::Predictor$new(learner, data = task$data(), y = "Risk")
# Fit predictor
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_train)
# Create a predictor object
predictor = iml::Predictor$new(rf, type = "prob", data = data_train)

# Counterfactual fairness true counterfactuals

pred1 <- predictor$predict(data_test[which(data_test$Sex == "male"),])
pred2 <- predictor$predict(data_counterfactual[idxs,])

(abs(sum(pred1[,1]-pred2[,1])))/nrow(pred2)







set.seed(SEED)
idxs = which(data_test$Sex == "male")
cf_classif = CFClassif$new(predictor, protected = "Sex", n_generations = 50L)
cfactuals = cf_classif$find_counterfactuals(
  x_interest = data_test[idxs[1], ], desired_class = "female", desired_prob = c(0.5, 1)
)


cfactuals$predict(
)
cfactuals$x_interest
cfactuals$data

pred1 <- predictor$predict(cfactuals$x_interest)
pred2 <- predictor$predict(cfactuals$data)

# AMPD
(abs(sum(pred1[,1]-pred2[,1])))/nrow(pred2)

cf_classif <- CFClassif$new(predictor, protected = "Sex", n_generations = 50L)

# Initialize lists to store counterfactuals and predictions
cf_list <- list()
pred_list <- list()

# Loop through all instances in idxs
for (i in seq_along(idxs)) {
  cfactuals <- cf_classif$find_counterfactuals(
    x_interest = data_test[idxs[i], ], desired_class = "female", desired_prob = c(0.5, 1)
  )
  
  # Store counterfactuals and predictions
  cf_list[[i]] <- cfactuals
  pred1 <- predictor$predict(cfactuals$x_interest)
  pred2 <- predictor$predict(cfactuals$data)
  pred_list[[i]] <- list(pred1 = pred1, pred2 = pred2)
}

# Calculate AMPD for all instances
ampd_values <- sapply(pred_list, function(preds) {
  abs(sum(preds$pred1[, 1] - preds$pred2[, 1])) / nrow(preds$pred2)
})

# Print AMPD values
sum(ampd_values)/length(ampd_values)



#-----------#
# Data with unmeasured confounder
#-----------#


# Random Forest
SEED <- 123
generations = 30L
idxs = which(data_test$Sex == "male")

# prep data
data_train <- data_no_confounder_train # train data
data_test <- data_no_confounder_test # test data
# counterfactual data do(Sex = female)
data_counterfactual <- data_no_confounder_counterfactual_female 

# Fit predictor
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_train)
# Create a predictor object
predictor = iml::Predictor$new(rf, type = "prob", data = data_train)

# Counterfactual fairness true counterfactuals

pred1 <- predictor$predict(data_test[which(data_test$Sex == "male"),])
pred2 <- predictor$predict(data_counterfactual[idxs,])

(abs(sum(pred1[,1]-pred2[,1])))/nrow(pred2)







set.seed(SEED)
idxs = which(data_test$Sex == "male")
cf_classif = CFClassif$new(predictor, protected = "Sex", n_generations = 50L)
cfactuals = cf_classif$find_counterfactuals(
  x_interest = data_test[idxs[1], ], desired_class = "female", desired_prob = c(0.5, 1)
)


cfactuals$predict(
)
cfactuals$x_interest
cfactuals$data

pred1 <- predictor$predict(cfactuals$x_interest)
pred2 <- predictor$predict(cfactuals$data)

# AMPD
(abs(sum(pred1[,1]-pred2[,1])))/nrow(pred2)

cf_classif <- CFClassif$new(predictor, protected = "Sex", n_generations = 50L)

# Initialize lists to store counterfactuals and predictions
cf_list <- list()
pred_list <- list()

# Loop through all instances in idxs
for (i in seq_along(idxs)) {
  cfactuals <- cf_classif$find_counterfactuals(
    x_interest = data_test[idxs[i], ], desired_class = "female", desired_prob = c(0.5, 1)
  )
  
  # Store counterfactuals and predictions
  cf_list[[i]] <- cfactuals
  pred1 <- predictor$predict(cfactuals$x_interest)
  pred2 <- predictor$predict(cfactuals$data)
  pred_list[[i]] <- list(pred1 = pred1, pred2 = pred2)
}

# Calculate AMPD for all instances
ampd_values <- sapply(pred_list, function(preds) {
  abs(sum(preds$pred1[, 1] - preds$pred2[, 1])) / nrow(preds$pred2)
})

# Print AMPD values
sum(ampd_values)/length(ampd_values)



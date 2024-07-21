library(data.table)
library(mlr3)
library(mlr3learners)
library(iml)
source("code/sim_data.R")


data <- as.data.table(data_no_confounder_train)
data_ftu_train <- data[, !"Sex", with = FALSE]
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_ftu_train, target = "Risk")
learner$predict_type = "prob"
set.seed(142)
learner$train(task)
predictor = iml::Predictor$new(learner, data = task$data(), y = "Risk")

mean(predictor$predict(data.table(data_no_confounder_test))[,1] - 
       predictor$predict(data.table(data_no_confounder_counterfactual))[,1])

idxs = which(data$Sex == "male")
gen_cf_classif = function(data, xr, idx, vars) {
  cf_classif = CFClassif$new(predictor, protected = "race", n_generations = 30L)
  cfactuals = cf_classif$find_counterfactuals(
    x_interest = data[idx, ], desired_class = "White", desired_prob = c(0.5, 1)
  )
  cfactuals$subset_to_valid()

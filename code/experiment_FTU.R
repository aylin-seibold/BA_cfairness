rm(list=ls())
library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
devtools::load_all("extern/counterfactuals")
source("code/sim_data.R")
source("code/utils.R")

SEED <- 123
generations = 30L


#-----------#
# Data without unmeasured confounder
#-----------#

### Data ##
data_no_confounder_train # train data
data_no_confounder_train[, Sex := NULL]
data_no_confounder_test # test data
data_no_confounder_counterfactual_female # counterfactual data do(Sex = female)

idxs = which(data_no_confounder_test$Sex == "male") # row id for Sex = male

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_no_confounder_train, target = "Risk")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_lg = iml::Predictor$new(learner, data = data_no_confounder_test, y = "Risk")

# Random Forest
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_no_confounder_train)
# Predictor Object
predictor_rf = iml::Predictor$new(rf, type = "prob", data = data_no_confounder_test)


# AMPD and MBE for true Counterfactuals

# With Logistic Regression
calculate_ampd_mbe(predictor_rf, data_no_confounder_test, data_no_confounder_counterfactual_female, idxs)

# With Random Forest
calculate_ampd_mbe(predictor_lg, data_no_confounder_test, data_no_confounder_counterfactual_female, idxs)


# generate mocf counterfactuals and calculate AMPD and MBE

# With Logistic Regression
calculate_ampd_mbe_moc(predictor_rf, data_no_confounder_test, idxs)

# With Random Forest
calculate_ampd_mbe_moc(predictor_lg, data_no_confounder_test, idxs)




#-----------#
# Data with unmeasured confounder
#-----------#

### Data ###

data_confounder_train # train data
data_confounder_train[, Sex := NULL]
data_confounder_test # test data
data_confounder_counterfactual_female # counterfactual data do(Sex = female)

idxs = which(data_confounder_test$Sex == "male") # row id for Sex = male

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_confounder_train, target = "Risk")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_lg = iml::Predictor$new(learner, data = data_confounder_test, y = "Risk")

# Random Forest
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_confounder_train)
# Predictor Object
predictor_rf = iml::Predictor$new(rf, type = "prob", data = data_confounder_test)


# AMPD and MBE for true Counterfactuals

# With Logistic Regression
calculate_ampd_mbe(predictor_rf, data_confounder_test, data_confounder_counterfactual_female, idxs)

# With Random Forest
calculate_ampd_mbe(predictor_lg, data_confounder_test, data_confounder_counterfactual_female, idxs)


# generate mocf counterfactuals and calculate AMPD and MBE

# With Logistic Regression
calculate_ampd_mbe_moc(predictor_rf, data_confounder_test, idxs)

# With Random Forest
calculate_ampd_mbe_moc(predictor_lg, data_confounder_test, idxs)

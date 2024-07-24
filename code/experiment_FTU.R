rm(list=ls())
library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
devtools::load_all("extern/counterfactuals")
source("code/sim_data1.R")
source("code/utils.R")

SEED <- 123
generations = 30L

#-----------#
# Data without unmeasured confounder
#-----------#

### Data ##
# data_no_confounder_train # train data
# data_no_confounder_test # test data
# data_no_confounder_counterfactual_female # counterfactual data do(Sex = female)
# data_no_confounder_counterfactual_male # counterfactual data do(Sex = male)

data_no_confounder_train[, Sex := NULL]

idxs = which(data_no_confounder_test$Sex == "male") # row id for Sex = male in test data

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_no_confounder_train, target = "Risk")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_no_confounding_lg = iml::Predictor$new(learner, data = data_no_confounder_test, y = "Risk")

# Random Forest
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_no_confounder_train)
# Predictor Object
predictor_no_confounding_rf = iml::Predictor$new(rf, type = "prob", data = data_no_confounder_test)



### AMPD and MBE for true Counterfactuals ###
# With Logistic Regression
res_true_no_confounding_rf <- calculate_ampd_mbe(predictor_no_confounding_rf, data_no_confounder_counterfactual_male, data_no_confounder_counterfactual_female, idxs)

# With Random Forest
res_true_no_confounding_lg <- calculate_ampd_mbe(predictor_no_confounding_lg, data_no_confounder_counterfactual_male, data_no_confounder_counterfactual_female, idxs)


### generate mocf counterfactuals and calculate AMPD and MBE ###
# With Logistic Regression
predictor <- predictor_no_confounding_rf
res_gen_no_confounding_rf <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)

# With Random Forest
predictor <- predictor_no_confounding_lg
res_gen_no_confounding_lg <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)




#-----------#
# Data with unmeasured confounder
#-----------#

#-----------#
# Data without unmeasured confounder
#-----------#

### Data ##
# data_confounder_train # train data
# data_confounder_test # test data
# data_confounder_counterfactual_female # counterfactual data do(Sex = female)
# data_confounder_counterfactual_male # counterfactual data do(Sex = male)

data_confounder_train[, Sex := NULL]

idxs = which(data_confounder_test$Sex == "male") # row id for Sex = male in test data

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_confounder_train, target = "Risk")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_confounding_lg = iml::Predictor$new(learner, data = data_confounder_test, y = "Risk")

# Random Forest
set.seed(SEED)
rf = randomForest(Risk ~ ., data = data_confounder_train)
# Predictor Object
predictor_confounding_rf = iml::Predictor$new(rf, type = "prob", data = data_confounder_test)



### AMPD and MBE for true Counterfactuals ###
# With Logistic Regression
res_true_confounding_rf <- calculate_ampd_mbe(predictor_confounding_rf, data_confounder_counterfactual_male, data_confounder_counterfactual_female, idxs)

# With Random Forest
res_true_confounding_lg <- calculate_ampd_mbe(predictor_confounding_lg, data_confounder_counterfactual_male, data_confounder_counterfactual_female, idxs)


### generate mocf counterfactuals and calculate AMPD and MBE ###
# With Logistic Regression
predictor <- predictor_confounding_rf
res_gen_confounding_rf <- calculate_ampd_mbe_moc(predictor, data_confounder_test, idxs)

# With Random Forest
predictor <- predictor_confounding_lg
res_gen_confounding_lg <- calculate_ampd_mbe_moc(predictor, data_confounder_test, idxs)


### Save Results ###
### Save Results ###
results_ftu <- list(res_true_no_confounding_lg = res_true_no_confounding_lg, 
                      res_true_no_confounding_rf = res_true_no_confounding_rf,
                      res_gen_no_confounding_lg = res_gen_no_confounding_lg, 
                      res_gen_no_confounding_rf = res_gen_no_confounding_rf,
                      res_true_confounding_lg = res_true_confounding_lg, 
                      res_true_confounding_rf = res_true_confounding_rf, 
                      res_gen_confounding_lg = res_gen_confounding_lg,
                      res_gen_confounding_rf = res_gen_confounding_rf)

saveRDS(results_ftu, file="intermediate/results_ftu.Rda")

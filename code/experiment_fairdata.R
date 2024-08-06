library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
devtools::load_all("extern/counterfactuals")
source("code/sim_data_fair.R") # Load fair data
source("code/utils.R") # Funktions for calculating AMPD and MBE

#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. Calculations for data without confounder
# 1.1 Make predictor objects
# 1.2 MBE and AMPD for true counterfactuals
# 1.2 MBE and AMPD for generated counterfactuals
# 2. Calculations for data with confounder
# 2.1 Make predictor objects
# 2.2 MBE and AMPD for true counterfactuals
# 2.2 MBE and AMPD for generated counterfactuals
#----------------------------------#

SEED <- 123
generations = 30L

#----------------------------------#
#### 1. Calculations for data without confounder ####
#----------------------------------#

#----------------------------------#
#### 1.1 Make predictor objects ####
#----------------------------------#

# Logistic regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_no_confounder_train, target = "Y")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
predictor_no_confounding_lg = iml::Predictor$new(learner, data = data_no_confounder_test, y = "Y")

# Random forest
set.seed(SEED)
rf = randomForest(Y ~ ., data = data_no_confounder_train)
predictor_no_confounding_rf = iml::Predictor$new(rf, type = "prob", data = data_no_confounder_test)

#----------------------------------#
#### 1.2 MBE and AMPD for true counterfactuals ####
#----------------------------------#

idxs = which(data_no_confounder_test$A == "1") # id for protected class = 1
# Lists for MBE and AMPD values 
res_true_no_confounding_rf <- calculate_ampd_mbe(predictor_no_confounding_rf, data_no_confounder_counterfactual_class1, data_no_confounder_counterfactual_class0, idxs)
res_true_no_confounding_lg <- calculate_ampd_mbe(predictor_no_confounding_lg, data_no_confounder_counterfactual_class1, data_no_confounder_counterfactual_class0, idxs)

#----------------------------------#
#### 1.2 MBE and AMPD for generated counterfactuals ####
#----------------------------------#
predictor <- predictor_no_confounding_rf
res_gen_no_confounding_rf <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)
predictor <- predictor_no_confounding_lg
res_gen_no_confounding_lg <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)

#----------------------------------#
#### 2. Calculations for data with confounder ####
#----------------------------------#

#----------------------------------#
#### 2.1 Make predictor objects ####
#----------------------------------#

# Logistic regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_confounder_s_train, target = "Y")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
predictor_confounding_s_lg = iml::Predictor$new(learner, data = data_confounder_s_test, y = "Y")

# Random forest
set.seed(SEED)
rf = randomForest(Y ~ ., data = data_confounder_s_train)
# Predictor Object
predictor_confounding_s_rf = iml::Predictor$new(rf, type = "prob", data = data_confounder_s_test)

#----------------------------------#
#### 2.2 MBE and AMPD for true counterfactuals ####
#----------------------------------#

idxs = which(data_confounder_s_test$A == "1") # id for protected class = 1
res_true_confounding_s_rf <- calculate_ampd_mbe(predictor_confounding_s_rf, data_confounder_s_counterfactual_class1, data_confounder_s_counterfactual_class0, idxs)
res_true_confounding_s_lg <- calculate_ampd_mbe(predictor_confounding_s_lg, data_confounder_s_counterfactual_class1, data_confounder_s_counterfactual_class0, idxs)

#----------------------------------#
#### 2.2 MBE and AMPD for generated counterfactuals ####
#----------------------------------#
predictor <- predictor_confounding_s_rf
res_gen_confounding_s_rf <- calculate_ampd_mbe_moc(predictor, data_confounder_s_test, idxs)
predictor <- predictor_confounding_s_lg
res_gen_confounding_s_lg <- calculate_ampd_mbe_moc(predictor, data_confounder_s_test, idxs)

### Save Results ###
results_fairdata <- list(res_true_no_confounding_lg = res_true_no_confounding_lg, 
                      res_true_no_confounding_rf = res_true_no_confounding_rf,
                      res_gen_no_confounding_lg = res_gen_no_confounding_lg, 
                      res_gen_no_confounding_rf = res_gen_no_confounding_rf,
                      res_true_confounding_s_lg = res_true_confounding_s_lg, 
                      res_true_confounding_s_rf = res_true_confounding_s_rf,
                      res_gen_confounding_s_lg = res_gen_confounding_s_lg,
                      res_gen_confounding_s_rf = res_gen_confounding_s_rf)
saveRDS(results_fairdata, file="intermediate/results_fairdata.Rda")

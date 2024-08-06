library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
devtools::load_all("extern/counterfactuals")
source("code/sim_data_unfair.R") # Load unfair data
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
# Residuals train data
# Residuals for X1
model_sav <- lm(X1 ~ A, data = data_no_confounder_train)
data_no_confounder_train$resi_sav <- data_no_confounder_train$X1 - predict(model_sav, newdata = data_no_confounder_train)
# Extract protected attribute and X1 to fit predictors only on residuals and X2
data_no_confounder_train[, c("A", "X1") := NULL]

# Residuals test data
# Residuals for X1
model_sav <- lm(X1 ~ A, data = data_no_confounder_test)
data_no_confounder_test$resi_sav <- data_no_confounder_test$X1 - predict(model_sav, newdata = data_no_confounder_test)

# Residuals true counterfactual data
data_counterfactual <- rbind(data_no_confounder_counterfactual_class0, data_no_confounder_counterfactual_class1)
# Residuals for X1
model_sav <- lm(X1 ~ A, data = data_counterfactual)
data_counterfactual$resi_sav <- data_counterfactual$X1 - predict(model_sav, newdata = data_counterfactual)
data_no_confounder_counterfactual_class0 <- data_counterfactual[which(A == "0"),]
data_no_confounder_counterfactual_class1 <- data_counterfactual[which(A == "1"),]

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

# Residuals train data
# Residuals for X1
model_sav <- lm(X1 ~ A, data = data_confounder_s_train)
data_confounder_s_train$resi_sav <- data_confounder_s_train$X1 - predict(model_sav, newdata = data_confounder_s_train)
data_confounder_s_train[, c("A", "X1") := NULL]

# Residuals test data
# Residuals for X1
modes_sav <- lm(X1 ~ A, data = data_confounder_s_test)
data_confounder_s_test$resi_sav <- data_confounder_s_test$X1 - predict(model_sav, newdata = data_confounder_s_test)

# Residuals counterfactual data
# Residuals for X1
data_counterfactual <- rbind(data_no_confounder_counterfactual_class0, data_no_confounder_counterfactual_class1)
model_sav <- lm(X1 ~ A, data = data_counterfactual)
data_counterfactual$resi_sav <- data_counterfactual$X1 - predict(model_sav, newdata = data_counterfactual)
data_confounder_s_counterfactual_class0 <- data_counterfactual[which(A == "0"),]
data_confounder_s_counterfactual_class1 <- data_counterfactual[which(A == "1"),]

#----------------------------------#
#### 2.1 Make predictor objects ####
#----------------------------------#

# Logistic Regression
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
predictor_confounding_s_rf = iml::Predictor$new(rf, type = "prob", data = data_confounder_s_test)

#----------------------------------#
#### 2.2 MBE and AMPD for true counterfactuals ####
#----------------------------------#

idxs = which(data_confounder_s_test$A == "1")  # id for protected class = 1
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
results_fairadd <- list(res_true_no_confounding_lg = res_true_no_confounding_lg, 
                      res_true_no_confounding_rf = res_true_no_confounding_rf,
                      res_gen_no_confounding_lg = res_gen_no_confounding_lg, 
                      res_gen_no_confounding_rf = res_gen_no_confounding_rf,
                      res_true_confounding_s_lg = res_true_confounding_s_lg, 
                      res_true_confounding_s_rf = res_true_confounding_s_rf,
                      res_gen_confounding_s_lg = res_gen_confounding_s_lg,
                      res_gen_confounding_s_rf = res_gen_confounding_s_rf)
saveRDS(results_fairadd, file="intermediate/results_fairadd.Rda")

rm(list=ls())
library("iml")
library("mlr3")
library("mlr3pipelines")
library("mlr3learners")
library("mlr3fairness")
library("randomForest")
devtools::load_all("extern/counterfactuals")
source("code/sim_data_unfair.R")
source("code/utils.R")

SEED <- 123
generations = 30L

#-----------#
# Data without unmeasured confounder
#-----------#

# Resids train data
# resids Saving
model_sav <- lm(X1 ~ A, data = data_no_confounder_train)
data_no_confounder_train$resi_sav <- data_no_confounder_train$X1 - predict(model_sav, newdata = data_no_confounder_train)

data_no_confounder_train[, c("A", "X1") := NULL]

# Resids test data
# resids Saving
model_sav <- lm(X1 ~ A, data = data_no_confounder_test)
data_no_confounder_test$resi_sav <- data_no_confounder_test$X1 - predict(model_sav, newdata = data_no_confounder_test)


data_counterfactual <- rbind(data_no_confounder_counterfactual_class0, data_no_confounder_counterfactual_class1)

# Resids counterfactual data 
# resids Saving
model_sav <- lm(X1 ~ A, data = data_counterfactual)
data_counterfactual$resi_sav <- data_counterfactual$X1 - predict(model_sav, newdata = data_counterfactual)

data_no_confounder_counterfactual_class0 <- data_counterfactual[which(A == "0"),]
data_no_confounder_counterfactual_class1 <- data_counterfactual[which(A == "1"),]




idxs = which(data_no_confounder_test$A == "1") # row id for Sex = male in test data

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_no_confounder_train, target = "Y")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_no_confounding_lg = iml::Predictor$new(learner, data = data_no_confounder_test, y = "Y")

# Random Forest
set.seed(SEED)
rf = randomForest(Y ~ ., data = data_no_confounder_train)
# Predictor Object
predictor_no_confounding_rf = iml::Predictor$new(rf, type = "prob", data = data_no_confounder_test)



### AMPD and MBE for true Counterfactuals ###
# With Logistic Regression
res_true_no_confounding_rf <- calculate_ampd_mbe(predictor_no_confounding_rf, data_no_confounder_counterfactual_class1, data_no_confounder_counterfactual_class0, idxs)

# With Random Forest
res_true_no_confounding_lg <- calculate_ampd_mbe(predictor_no_confounding_lg, data_no_confounder_counterfactual_class1, data_no_confounder_counterfactual_class0, idxs)


### generate mocf counterfactuals and calculate AMPD and MBE ###
# With Logistic Regression
predictor <- predictor_no_confounding_rf
res_gen_no_confounding_rf <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)

# With Random Forest
predictor <- predictor_no_confounding_lg
res_gen_no_confounding_lg <- calculate_ampd_mbe_moc(predictor, data_no_confounder_test, idxs)



#-----------#
# Data with unmeasured confounder 0.7
#-----------#




# Resids train data
# resids Saving
model_sav <- lm(X1 ~ A, data = data_confounder_s_train)
data_confounder_s_train$resi_sav <- data_confounder_s_train$X1 - predict(model_sav, newdata = data_confounder_s_train)

data_confounder_s_train[, c("A", "X1") := NULL]

# Resids test data
# resids Saving
modes_sav <- lm(X1 ~ A, data = data_confounder_s_test)
data_confounder_s_test$resi_sav <- data_confounder_s_test$X1 - predict(model_sav, newdata = data_confounder_s_test)


data_counterfactual <- rbind(data_no_confounder_counterfactual_class0, data_no_confounder_counterfactual_class1)

# Resids counterfactual data 
# resids Saving
model_sav <- lm(X1 ~ A, data = data_counterfactual)
data_counterfactual$resi_sav <- data_counterfactual$X1 - predict(model_sav, newdata = data_counterfactual)

data_confounder_s_counterfactual_class0 <- data_counterfactual[which(A == "0"),]
data_confounder_s_counterfactual_class1 <- data_counterfactual[which(A == "1"),]


idxs = which(data_confounder_s_test$A == "1") # row id for Sex = male in test data

### Fitting Predictors ###

# Logistic Regression
set.seed(SEED)
learner = mlr_learners$get("classif.log_reg")
task = as_task_classif(data_confounder_s_train, target = "Y")
learner$predict_type = "prob"
set.seed(SEED)
learner$train(task)
# Predictor Object
predictor_confounding_s_lg = iml::Predictor$new(learner, data = data_confounder_s_test, y = "Y")

# Random Forest
set.seed(SEED)
rf = randomForest(Y ~ ., data = data_confounder_s_train)
# Predictor Object
predictor_confounding_s_rf = iml::Predictor$new(rf, type = "prob", data = data_confounder_s_test)



### AMPD and MBE for true Counterfactuals ###
# With Logistic Regression
res_true_confounding_s_rf <- calculate_ampd_mbe(predictor_confounding_s_rf, data_confounder_s_counterfactual_class1, data_confounder_s_counterfactual_class0, idxs)

# With Random Forest
res_true_confounding_s_lg <- calculate_ampd_mbe(predictor_confounding_s_lg, data_confounder_s_counterfactual_class1, data_confounder_s_counterfactual_class0, idxs)


### generate mocf counterfactuals and calculate AMPD and MBE ###
# With Logistic Regression
predictor <- predictor_confounding_s_rf
res_gen_confounding_s_rf <- calculate_ampd_mbe_moc(predictor, data_confounder_s_test, idxs)

# With Random Forest
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

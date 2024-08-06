library(simcausal)
library(data.table)

#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. Preparation DAG without confounder 
# 2. Preparation DAG with confounder
# 3. Simulation Data without confounder
# 4. Simulation DAG with confounder
#----------------------------------#

n_train <- 1000 # 2000 # 10000
n_test <- 200 # 400 # 2000
SEED <- 123

#----------------------------------#
#### 1. Preparation DAG without confounder  ####
#----------------------------------#

D_no_confounder <- DAG.empty()
D_no_confounder <- D_no_confounder + 
  node("A", 
       distr = "rbern", prob = 0.5)+
  node("X1",
       distr = "rbern", prob = plogis(4 - 1.25))+
  node("X2", 
       distr = "rgamma", scale = 0.74*exp(7.9), shape = 1/0.74)+
  node("Y", 
       distr = "rbern", prob = plogis(0.9 - 1*X1 - 0.001*X2))

#plotDAG(set.DAG(D_no_confounder))
D_no_confounder_set <- set.DAG(D_no_confounder)

#----------------------------------#
#### 2. Preparation DAG with confounder  ####
#----------------------------------#

D_confounder_s <- DAG.empty()
D_confounder_s <- D_confounder_s +
  node("C",
       distr = "rnorm", mean = 0, sd = 1) +
  node("A", 
       distr = "rbern", prob = plogis(0.5 + 0.7*C))+
  node("X1", 
       distr = "rbern", prob = plogis(4 + 1.25))+
  node("X2", 
       distr = "rgamma", scale = 0.74*exp(7.9 - 0.7*C), shape = 1/0.74)+
  node("Y", 
       distr = "rbern", prob = plogis(0.9 + 1*X1 - 0.005*X2))
# plotDAG(set.DAG(D_confounder_s))
D_confounder_s_set <- set.DAG(D_confounder_s)


#----------------------------------#
#### 3. Simulation Data without confounder  ####
#----------------------------------#

# Train data
data_no_confounder_train <- simcausal::sim(DAG = D_no_confounder_set,
                                           n = n_train,
                                           rndseed = SEED,
                                           verbose = FALSE)


data_no_confounder_train <- data_no_confounder_train[,-1]
data_no_confounder_train$A <- factor(data_no_confounder_train$A, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_train$Y <- factor(data_no_confounder_train$Y, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_train <- data.table(data_no_confounder_train)


# Test data and counterfactual data
I <- node("A", distr = "rbern", prob = 0.5)
D_no_confounder_set <- D_no_confounder_set + action("I", nodes = I) # test data 
I1 <- node("A", distr = "rbern", prob = 1)
D_no_confounder_set <- D_no_confounder_set + action("I1", nodes = I1) # intervention -> class1
I0 <- node("A", distr = "rbern", prob = 0)
D_no_confounder_set <- D_no_confounder_set + action("I0", nodes = I0) # intervention -> class0

data_no_confounder_counterfactual <- simcausal::sim(DAG = D_no_confounder_set,
                                                    actions = c("I", "I1", "I0"),
                                                    n = n_test,
                                                    rndseed = SEED,
                                                    verbose = FALSE)

data_no_confounder_test <- data_no_confounder_counterfactual[["I"]]
data_no_confounder_test <- data_no_confounder_test[,-1]
data_no_confounder_test$A <- factor(data_no_confounder_test$A, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_test$Y <- factor(data_no_confounder_test$Y, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_test <- data.table(data_no_confounder_test)


data_no_confounder_counterfactual_class0 <- data_no_confounder_counterfactual[["I0"]]
data_no_confounder_counterfactual_class0 <- data_no_confounder_counterfactual_class0[,-1]
data_no_confounder_counterfactual_class0$A <- factor(data_no_confounder_counterfactual_class0$A, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_counterfactual_class0$Y <- factor(data_no_confounder_counterfactual_class0$Y, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_counterfactual_class0 <- data.table(data_no_confounder_counterfactual_class0)


data_no_confounder_counterfactual_class1 <- data_no_confounder_counterfactual[["I1"]]
data_no_confounder_counterfactual_class1 <- data_no_confounder_counterfactual_class1[,-1]
data_no_confounder_counterfactual_class1$A <- factor(data_no_confounder_counterfactual_class1$A, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_counterfactual_class1$Y <- factor(data_no_confounder_counterfactual_class1$Y, levels = c(0:1), labels = c("0", "1"))
data_no_confounder_counterfactual_class1 <- data.table(data_no_confounder_counterfactual_class1)


#----------------------------------#
#### 3. Simulation Data with confounder  ####
#----------------------------------#

data_confounder_s_train <- simcausal::sim(DAG = D_confounder_s_set,
                                          n = n_train,
                                          rndseed = SEED,
                                          verbose = FALSE)

data_confounder_s_train <- data_confounder_s_train[,-1]
data_confounder_s_train$A <- factor(data_confounder_s_train$A, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_train$Y <- factor(data_confounder_s_train$Y, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_train <- data.table(data_confounder_s_train)
data_confounder_s_train[, C := NULL]


# test data + counterfactual data

I <- node("A", distr = "rbern", prob = plogis(0.5 + 0.7*C))
D_confounder_s_set <- D_confounder_s_set + action("I", nodes = I) # no intervention
I1 <- node("A", distr = "rbern", prob = 1)
D_confounder_s_set <- D_confounder_s_set + action("I1", nodes = I1)
I0 <- node("A", distr = "rbern", prob = 0)
D_confounder_s_set <- D_confounder_s_set + action("I0", nodes = I0)

data_confounder_s_counterfactual <- simcausal::sim(DAG = D_confounder_s_set,
                                                   actions = c("I", "I1", "I0"),
                                                   n = n_test,
                                                   rndseed = SEED,
                                                   verbose = FALSE)

data_confounder_s_test <- data_confounder_s_counterfactual[["I"]]
data_confounder_s_test <- data_confounder_s_test[,-1]
data_confounder_s_test$A <- factor(data_confounder_s_test$A, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_test$Y <- factor(data_confounder_s_test$Y, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_test <- data.table(data_confounder_s_test)
data_confounder_s_test[, C := NULL]

data_confounder_s_counterfactual_class0 <- data_confounder_s_counterfactual[["I0"]]
data_confounder_s_counterfactual_class0 <- data_confounder_s_counterfactual_class0[,-1]
data_confounder_s_counterfactual_class0$A <- factor(data_confounder_s_counterfactual_class0$A, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_counterfactual_class0$Y <- factor(data_confounder_s_counterfactual_class0$Y, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_counterfactual_class0 <- data.table(data_confounder_s_counterfactual_class0)
data_confounder_s_counterfactual_class0[, C := NULL]

data_confounder_s_counterfactual_class1 <- data_confounder_s_counterfactual[["I1"]]
data_confounder_s_counterfactual_class1 <- data_confounder_s_counterfactual_class1[,-1]
data_confounder_s_counterfactual_class1$A <- factor(data_confounder_s_counterfactual_class1$A, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_counterfactual_class1$Y <- factor(data_confounder_s_counterfactual_class1$Y, levels = c(0:1), labels = c("0", "1"))
data_confounder_s_counterfactual_class1 <- data.table(data_confounder_s_counterfactual_class1)
data_confounder_s_counterfactual_class1[, C := NULL]


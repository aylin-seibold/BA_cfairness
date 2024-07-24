# TODO: still needs to be commented out

library(simcausal)
library(data.table)

n_train <- 1000 # 5000 # 10000
n_test <- 200 # 1000 # 2000
SEED <- 123

#-----------#
# Prepare DAGs
#-----------#

#-----------#
# DAG without unmeasured confounder
#-----------#

D_no_confounder <- DAG.empty()

D_no_confounder <- D_no_confounder +
  node("Sex", # Sex
       distr = "rbern", prob = 0.69) +
  node("Saving", # Saving | Sex, U
       distr = "rbern", prob = plogis(4 - 1.25 * Sex)) +
  node("Amount", # Amount | Sex, U
       distr = "rgamma", scale = 0.74*exp(7.9 + 0.175*Sex), shape = 1/0.74) +
  node("Risk", # Risk | Sex, Saving, Amount
       distr = "rbern", prob = plogis(0.9 + 1.75*Sex - 0.7*Saving - 0.001*Amount))

plotDAG(set.DAG(D_no_confounder))

D_no_confounder_set <- set.DAG(D_no_confounder)

#-----------#
# DAG with unmeasured confounder
#-----------#

D_confounder <- DAG.empty()

D_confounder <- D_confounder +
  node("Sex", # Sex
       distr = "rbern", prob = 0.69) +
  node("C", # Latent Confounder C
       distr = "rnorm", mean = 0, sd = 1) +
  node("Saving", # Saving | Sex, U
       distr = "rbern", prob = plogis(4 - 1.25 * Sex + 1.5*C)) +
  node("Amount", # Amount | Sex, U
       distr = "rgamma", scale = 0.74*exp(7.9 + 0.175*Sex + 1.5*C), shape = 1/0.74) +
  node("Risk", # Risk | Sex, Saving, Amount
       distr = "rbern", prob = plogis(0.9 + 1.75*Sex - 0.7*Saving - 0.001*Amount))

plotDAG(set.DAG(D_confounder))

D_confounder_set <- set.DAG(D_confounder)


#-----------#
# Simulate Data
#-----------#

#-----------#
# Sim: DAG without unmeasured confounder
#-----------#

# train data
data_no_confounder_train <- simcausal::sim(DAG = D_no_confounder_set,
                                           n = n_train,
                                           rndseed = SEED,
                                           verbose = FALSE)

data_no_confounder_train <- data_no_confounder_train[,-1]
data_no_confounder_train$Sex <- factor(data_no_confounder_train$Sex, levels = c(0:1), labels = c("female", "male"))
data_no_confounder_train$Risk <- factor(data_no_confounder_train$Risk, levels = c(0:1), labels = c("bad", "good"))
data_no_confounder_train <- data.table(data_no_confounder_train)

# test data + counterfactual data

A <- node("Sex", distr = "rbern", prob = 0.69)
D_no_confounder_set <- D_no_confounder_set + action("A", nodes = A) # no intervention
A1 <- node("Sex", distr = "rbern", prob = 1)
D_no_confounder_set <- D_no_confounder_set + action("A1", nodes = A1) # intervention -> female
A0 <- node("Sex", distr = "rbern", prob = 0)
D_no_confounder_set <- D_no_confounder_set + action("A0", nodes = A0) # intervention -> male

data_no_confounder_counterfactual <- simcausal::sim(DAG = D_no_confounder_set,
                                                    actions = c("A", "A1", "A0"),
                                                    n = n_test,
                                                    rndseed = SEED,
                                                    verbose = FALSE)

data_no_confounder_test <- data_no_confounder_counterfactual[["A"]]
data_no_confounder_test <- data_no_confounder_test[,-1]
data_no_confounder_test$Sex <- factor(data_no_confounder_test$Sex, levels = c(0:1), labels = c("female", "male"))
data_no_confounder_test$Risk <- factor(data_no_confounder_test$Risk, levels = c(0:1), labels = c("bad", "good"))
data_no_confounder_test <- data.table(data_no_confounder_test)


data_no_confounder_counterfactual_female<- data_no_confounder_counterfactual[["A0"]]
data_no_confounder_counterfactual_female<- data_no_confounder_counterfactual_female[,-1]
data_no_confounder_counterfactual_female$Sex <- factor(data_no_confounder_counterfactual_female$Sex, levels = c(0:1), labels = c("female", "male"))
data_no_confounder_counterfactual_female$Risk <- factor(data_no_confounder_counterfactual_female$Risk, levels = c(0:1), labels = c("bad", "good"))
data_no_confounder_counterfactual_female <- data.table(data_no_confounder_counterfactual_female)


data_no_confounder_counterfactual_male<- data_no_confounder_counterfactual[["A1"]]
data_no_confounder_counterfactual_male<- data_no_confounder_counterfactual_male[,-1]
data_no_confounder_counterfactual_male$Sex <- factor(data_no_confounder_counterfactual_male$Sex, levels = c(0:1), labels = c("female", "male"))
data_no_confounder_counterfactual_male$Risk <- factor(data_no_confounder_counterfactual_male$Risk, levels = c(0:1), labels = c("bad", "good"))
data_no_confounder_counterfactual_male <- data.table(data_no_confounder_counterfactual_male)


#-----------#
# Sim: DAG with unmeasured confounder
#-----------#

data_confounder_train <- simcausal::sim(DAG = D_confounder_set,
                                        n = n_train,
                                        rndseed = SEED,
                                        verbose = FALSE)

data_confounder_train <- data_confounder_train[,-1]
data_confounder_train$Sex <- factor(data_confounder_train$Sex, levels = c(0:1), labels = c("female", "male"))
data_confounder_train$Risk <- factor(data_confounder_train$Risk, levels = c(0:1), labels = c("bad", "good"))
data_confounder_train <- data.table(data_confounder_train)
data_confounder_train[, C := NULL]


# test data + counterfactual data

A <- node("Sex", distr = "rbern", prob = 0.69)
D_confounder_set <- D_confounder_set + action("A", nodes = A) # no intervention
A1 <- node("Sex", distr = "rbern", prob = 1)
D_confounder_set <- D_confounder_set + action("A1", nodes = A1)
A0 <- node("Sex", distr = "rbern", prob = 0)
D_confounder_set <- D_confounder_set + action("A0", nodes = A0)

data_confounder_counterfactual <- simcausal::sim(DAG = D_confounder_set,
                                                    actions = c("A", "A1", "A0"),
                                                    n = n_test,
                                                    rndseed = SEED,
                                                    verbose = FALSE)

data_confounder_test <- data_confounder_counterfactual[["A"]]
data_confounder_test <- data_confounder_test[,-1]
data_confounder_test$Sex <- factor(data_confounder_test$Sex, levels = c(0:1), labels = c("female", "male"))
data_confounder_test$Risk <- factor(data_confounder_test$Risk, levels = c(0:1), labels = c("bad", "good"))
data_confounder_test <- data.table(data_confounder_test)
data_confounder_test[, C := NULL]

data_confounder_counterfactual_female <- data_confounder_counterfactual[["A0"]]
data_confounder_counterfactual_female <- data_confounder_counterfactual_female[,-1]
data_confounder_counterfactual_female$Sex <- factor(data_confounder_counterfactual_female$Sex, levels = c(0:1), labels = c("female", "male"))
data_confounder_counterfactual_female$Risk <- factor(data_confounder_counterfactual_female$Risk, levels = c(0:1), labels = c("bad", "good"))
data_confounder_counterfactual_female <- data.table(data_confounder_counterfactual_female)
data_confounder_counterfactual_female[, C := NULL]

data_confounder_counterfactual_male <- data_confounder_counterfactual[["A1"]]
data_confounder_counterfactual_male <- data_confounder_counterfactual_male[,-1]
data_confounder_counterfactual_male$Sex <- factor(data_confounder_counterfactual_male$Sex, levels = c(0:1), labels = c("female", "male"))
data_confounder_counterfactual_male$Risk <- factor(data_confounder_counterfactual_male$Risk, levels = c(0:1), labels = c("bad", "good"))
data_confounder_counterfactual_male <- data.table(data_confounder_counterfactual_male)
data_confounder_counterfactual_male[, C := NULL]

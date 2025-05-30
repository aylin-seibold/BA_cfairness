library(data.table)
library(randomForest)

test_that("whatif_algo returns correct counterfactuals", {
  rf = get_rf_classif_iris()
  ps = make_param_set(iris, lower = NULL, upper = NULL)
  mod = Predictor$new(rf, data = iris, y = "Species")
  x_interest = iris[1L, -5L]
  desired = c(0.7, 1)
  n = 5L
  res = whatif_algo(
    predictor = mod,
    n_cfactuals = n,
    x_interest = x_interest,
    pred_column = "versicolor",
    desired_y_hat_range = desired,
    X_search = mod$data$X
  )
  
  expect_data_table(res, nrows = n, types = sapply(iris[, -5L], class))
  expect_names(names(res), identical.to = names(iris[, -5L]))
  expect_numeric(mod$predict(res)[["versicolor"]], lower = desired[1L], upper = desired[2L])
  
  versicolor_only = subset(iris, Species == "versicolor", select = -Species)
  dist_vector = as.vector(gower_dist(x_interest, versicolor_only))
  cf_expected = as.data.table(head(versicolor_only[order(dist_vector), ], n))
  expect_identical(res, cf_expected)
})


test_that("whatif_algo returns warning and empty data.table with correct columns, if no counterfactuals were found", {
  set.seed(45415415)
  x = data.table(a = 1.5, b = 10)
  data = data.table(a = c(0, 2), b = c(0, 15), c = runif(10))
  rf = randomForest(c ~ ., data)
  mod = Predictor$new(rf, data, y = "c")
  
  expect_snapshot({
    res = whatif_algo(
      predictor = mod,
      n_cfactuals = 5L,
      x_interest = x_interest,
      pred_column = "pred",
      desired_y_hat_range = c(5, 10),
      X_search = mod$data$X
    )
  })
  
  expect_identical(res, data.table(a = numeric(0), b = numeric(0)))
})


test_that("whatif_algo search space is restricted to unique obervations", {
  iris_dupl = rbind(iris, iris)
  rf = randomForest::randomForest(Species ~ ., data = iris_dupl, ntree = 20L)
  ps = make_param_set(iris_dupl, lower = NULL, upper = NULL)
  mod = Predictor$new(rf, data = iris_dupl, y = "Species")
  x_interest = iris_dupl[1L, -5L]
  desired = c(0.7, 1)
  n = 5L
  res = whatif_algo(
    predictor = mod,
    n_cfactuals = n,
    x_interest = x_interest,
    pred_column = "versicolor",
    desired_y_hat_range = desired,
    X_search = mod$data$X
  )
  expect_identical(unique(res), res)
})




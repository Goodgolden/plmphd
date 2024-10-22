# Load testthat
library(testthat)
load_all()

## Assumes train_data, test_data, and new_data
## are available in the environment
## an example of simulated datasets are saved as
## the internal datasets R/sysdata.rda

## the testing set has 10 individuals with age from 3 to 18
test4 <- test0 %>%
  filter(time <= 4)
test5 <- test0 %>%
  filter(time <= 5)

## Test if the function runs without errors
##  and captures the snapshot of the output
test_that("people_like_thee runs without error and output is captured", {
  result <- people_like_thee(
    train_data = train0,
    test_data = test0,
    new_data = test4,
    outcome_var = "ht",
    time_var = "time",
    id_var = "id",
    ## by default it using the brokenstick model
    ## with for internal knots and linear piece-wise
    bks_fixed = "1 + bs(time, df = 5, degree = 1) * sex",
    bks_random = "1 + bs(time, df = 5, degree = 1) * sex",
    tmin = 4,
    tmax = 18,
    anchor_time = c(5, 10, 15),
    gamlss_formula = "ht ~ cs(time, df = 3)",
    gamlss_sigma = "~ cs(time, df = 1)",
    match_methods = "mahalanobis")

  # Capture the snapshot of the result
  expect_snapshot(result)

  # Optionally check for attributes
  expect_snapshot(attr(result, "brokenstick_model"))
})

# Test if the function handles missing or incorrect inputs and snapshots the errors
test_that("people_like_thee handles invalid inputs and captures error snapshots", {

  # Expect an error for missing train_data
  expect_snapshot_error(
    people_like_thee(
      train_data = NULL,
      test_data = test_data,
      new_data = new_data,
      outcome_var = "ht",
      time_var = "time",
      id_var = "id",
      anchor_time = c(1, 3),
      gamlss_formula = ht ~ time * sex,
      gamlss_sigma = ~ time,
      match_methods = "euclidean"
    )
  )

  # Expect an error for missing outcome_var
  expect_snapshot_error(
    people_like_thee(
      train_data = train_data,
      test_data = test_data,
      new_data = new_data,
      outcome_var = NULL,
      time_var = "time",
      id_var = "id",
      anchor_time = c(1, 3),
      gamlss_formula = ht ~ time * sex,
      gamlss_sigma = ~ time,
      match_methods = "euclidean"
    )
  )
})

# Test if the function handles different matching methods and captures the snapshot
test_that("people_like_thee handles different matching methods and captures snapshot", {
  result_mahalanobis <- people_like_thee(
    train_data = train_data,
    test_data = test_data,
    new_data = new_data,
    outcome_var = "ht",
    time_var = "time",
    id_var = "id",
    tmin = 0,
    tmax = 4,
    anchor_time = c(1, 3),
    gamlss_formula = ht ~ time * sex,
    gamlss_sigma = ~ time,
    match_methods = "mahalanobis"
  )

  # Capture the snapshot of the result with Mahalanobis distance
  expect_snapshot(result_mahalanobis)
})

# Test weighted regression and capture the snapshot
test_that("people_like_thee handles weighted regression and captures snapshot", {
  result_weighted <- people_like_thee(
    train_data = train_data,
    test_data = test_data,
    new_data = new_data,
    outcome_var = "ht",
    time_var = "time",
    id_var = "id",
    tmin = 0,
    tmax = 4,
    anchor_time = c(1, 3),
    gamlss_formula = ht ~ time * sex,
    gamlss_sigma = ~ time,
    match_methods = "euclidean",
    weight = TRUE
  )

  # Capture the snapshot of the weighted regression result
  expect_snapshot(result_weighted)
})


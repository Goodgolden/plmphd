# File: tests/testthat/test-plm_single.R
library(testthat)
library(here)
library(tidyverse)
library(plmphd)

# Load test data
load(here("R", "sysdata.rda"))
# Prepare training and testing datasets
train1 <- train0 %>%
  group_by(id) %>%
  summarise(ht0 = min(ht))

train2 <- full_join(train0, train1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

test1 <- test0 %>%
  group_by(id) %>%
  summarise(ht0 = min(ht))

test2 <- full_join(test0, test1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

# Define the parameters for PLM models
bsk <- c(3, 6, 9, 12)
lf <- "ht ~ sex + sex:ht0 + ht0"
gf <- "ht ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"
# Test the plm_single function with 30 matches for anchor time 1-15
test_that("plm_single with 30 matches and anchor time 1-15 works correctly", {
  set.seed(555)
  plm_s_n30 <- people_like_i(train_data = train2,
                          test_data = test2,
                          outcome_var = "ht",
                          time_var = "time",
                          id_var = "id",
                          tmin = 3,
                          tmax = 17,
                          brokenstick_knots = bsk,
                          anchor_time = 10,  # Using anchor time 10 as an example
                          linear_formula = lf,
                          gamlss_formula = gf,
                          gamlss_sigma = gs,
                          match_number = 30,
                          match_plot = FALSE,
                          predict_plot = FALSE)
  expect_snapshot(plm_s_n30)
})

# Test the plm_single function with 30 matches for anchor time 11
test_that("plm_single with match number 30 and anchor time 11 works correctly", {
  set.seed(555)
  plm_s_t11 <- people_like_i(train_data = train2,
                          test_data = test2,
                          outcome_var = "ht",
                          time_var = "time",
                          id_var = "id",
                          tmin = 3,
                          tmax = 17,
                          brokenstick_knots = bsk,
                          anchor_time = 11,  # Fixed anchor time 11
                          linear_formula = lf,
                          gamlss_formula = gf,
                          gamlss_sigma = gs,
                          match_number = 30,  # Fixed number of matches
                          match_plot = FALSE,
                          predict_plot = FALSE) %>%
    suppressMessages()

  expect_snapshot(plm_s_t11)
})


test_that("Error is thrown when both match_number and match_alpha are provided", {
  set.seed(333)
  expect_snapshot_error({
    plm_single(train_data = train2,
               test_data = test2,
               outcome_var = "ht",
               time_var = "time",
               id_var = "id",
               tmin = 3,
               tmax = 17,
               brokenstick_knots = bsk,
               anchor_time = 10,
               linear_formula = lf,
               gamlss_formula = gf,
               gamlss_sigma = gs,
               match_number = 30,   # Providing both match_number
               match_alpha = 0.95,  # and match_alpha, should throw an error
               match_plot = FALSE,
               predict_plot = FALSE)
  })
})

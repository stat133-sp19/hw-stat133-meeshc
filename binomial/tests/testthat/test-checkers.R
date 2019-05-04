context("Check checker functions")

test_that("check_prob succeeds with probability between 0 and 1", {
  expect_true(check_prob(.5))
  expect_true(check_prob(0))
  expect_true(check_prob(1))
  expect_true(check_prob(0.75))
})

test_that("check_prob succeeds only when prob is length 1", {
  expect_true(check_prob(.5))
  expect_error(check_prob(c(0.5, 1)))
})

test_that("check_prob returns error if prob is invalid", {
  expect_error(check_prob(-0.25))
  expect_error(check_prob(1.5))
})

test_that("check_trials succeeds with non-negative integer", {
  expect_true(check_trials(3))
  expect_true(check_trials(10))
  expect_true(check_trials(0))
})

test_that("check_trials succeeds only when trials is length 1", {
  expect_true(check_trials(3))
  expect_error(check_trials(c(3, 3, 2)))
})

test_that("check_trials returns error if trials is invalid", {
  expect_error(check_trials(-3))
  expect_error(check_trials(0.5))
})

test_that("check_success succeeds with valid number for successes", {
  expect_true(check_success(2, 3))
  expect_true(check_success(0, 1))
  expect_true(check_success(2, 2))
})

test_that("check_success succeeds even when success has length greater than 1", {
  expect_true(check_success(1:3, 4))
  expect_true(check_success(c(2, 2), 3))
})

test_that("check_success returns error if invalid success value", {
  expect_error(check_success(5, 4))
  expect_error(check_success(-1, 2))
  expect_error(check_success(1.5, 3))
})

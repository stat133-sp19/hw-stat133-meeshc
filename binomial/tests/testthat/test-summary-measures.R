context("Check summary measures")

test_that("aux_mean functions as expected", {
  expect_equal(aux_mean(5, 0.1), 0.5)
  expect_equal(aux_mean(3, 0.3), 0.9)
  expect_equal(aux_mean(2, 0.5), 1)
})

test_that("aux_variance functions as expected", {
  expect_equal(aux_variance(5, 0.5), 1.25)
  expect_equal(aux_variance(3, 0.3), 0.63)
  expect_equal(aux_variance(10, .25), 1.875)
})

test_that("aux_mode functions as expected", {
  expect_equal(aux_mode(5, 0.5), 3)
  expect_equal(aux_mode(5, 0.1), 0)
  expect_equal(aux_mode(10, 0.3), 3)
})

test_that("aux_skewness functions as expected", {
  expect_equal(aux_skewness(5, 0.5), 0)
  expect_equal(round(aux_skewness(10, 0.3), 3), 0.276)
  expect_equal(round(aux_skewness(10, 0.1), 3), 0.843)
})

test_that("aux_kurtosis functions as expected", {
  expect_equal(aux_kurtosis(5, 0.5), -0.4)
  expect_equal(round(aux_kurtosis(10, 0.3), 3), -0.124)
  expect_equal(round(aux_kurtosis(5, 0.1), 3), 1.022)
})

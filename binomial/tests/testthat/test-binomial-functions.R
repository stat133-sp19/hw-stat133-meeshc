context("Check binomial functions")

test_that("Bin choose functions as expected", {
  expect_equal(bin_choose(5, 3), 10)
  expect_equal(bin_choose(3, 2), 3)
  expect_equal(bin_choose(2, 2), 1)
})

test_that("Bin choose errors when k is greater than n", {
  expect_error(bin_choose(5, 6))
})

test_that("Bin probability functions as expected", {
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_equal(bin_probability(55, 100, 0.45), 0.01075277)
})

test_that("Bin probability functions even when success is of length greater than one", {
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
})

test_that("Bin probability sends an error when trials, prob, or success is invalid", {
  expect_error(bin_probability(2, -10, .3))
  expect_error(bin_probability(-1, 10, .5))
  expect_error(bin_probability(3, 5, 1.4))
})

test_that("Bin distribution functions as expected", {
  expect_is(bin_distribution(5, 0.5), c("bindis", "data.frame"))
  bd <- bin_distribution(5, 0.5)
  expect_equal(bd$probability[2], 0.15625)
  expect_equal(bd$probability[4], 0.31250)
  expect_equal(bd$success[1], 0)
})

test_that("Bin cumulative functions as expected", {
  bc <- bin_cumulative(5, .5)
  expect_is(bc, c("bincum", "data.frame"))
  expect_equal(bc$cumulative[2], 0.18750)
  expect_equal(bc$cumulative[3], 0.5)
})

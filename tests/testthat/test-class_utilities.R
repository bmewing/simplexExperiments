context("test-class_utilities")

test_that("init", {
  expect_error(experiment$new(k = -1), "whole number")
  expect_silent(experiment$new(k = 0))
  expect_silent(experiment$new(k = 1))
  expect_silent(experiment$new(k = 1e7))
})

test_that("valid_experiment", {
  exp = experiment$new(k = 2)
  setup = data.frame(x1 = c(60, 30, 65), x2 = c(20, 40, 80))
  expect_error(exp$generate_initial_simplex(method = "manual", data = setup), regexp = "not provided 2 treatments")
  exp$add_treatment()
  exp$add_treatment()
  expect_error(exp$generate_initial_simplex(method = "manual", data = setup), regexp = "must include at least one")
})

test_that("%between%", {
  expect_true(4 %between% NULL)
  expect_true(4 %between% c(1, 10))
  expect_true(4 %between% c(4, 10))
  expect_true(4 %between% c(1, 4))
  expect_true(4 %between% c(10, 1))
  expect_true(4 %between% c(10, 1, 4))
  expect_true(all(4:9 %between% c(1, 10)))
  expect_true(all(4:9 %between% c(9, 4)))
  expect_false(4 %between% 1:2)
  expect_false(all(c(4, 11) %between% c(1, 10)))
})

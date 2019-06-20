context("test-setup_experiment")

test_that("add_response", {
  exp = experiment$new(k = 3)
  exp$add_response(name = "ph", range = c(0, 5), value = function(y, h){ (5 - y) ^ 2}) #nolint
  exp$add_response(name = "profit", value = function(y, h){1 / (max(h) - y + 0.0001)}) #nolint
  expect_length(exp$responses, 2)
  expect_error(exp$add_response("ph"), regexp = "A response with that name already exists")
  expect_error(exp$add_response("ph2"), regexp = "You must provide a function")
  expect_error(exp$add_response("ph2", range = c(0, 5, 7)), regexp = "exactly two numeric values")
  expect_error(exp$add_response("ph2", range = c("low", "high")), regexp = "exactly two numeric values")
  expect_error(exp$add_response("ph2", weight = "+++"), regexp = "Treatment weight must be a single numeric vector")
  expect_error(exp$add_response("ph2", weight = c(1, 2)), regexp = "Treatment weight must be a single numeric vector")
  expect_error(exp$add_response(12), regexp = "Treatment name must be a single character vector")
  expect_error(exp$add_response(c("ph2", "ph3")), regexp = "Treatment name must be a single character vector")
})

test_that("drop_response", {
  exp = experiment$new(k = 3)
  exp$add_response(name = "ph", range = c(0, 5), value = function(y, h){ (5 - y) ^ 2}) #nolint
  exp$add_response(name = "profit", value = function(y, h){1 / (max(h) - y + 0.0001)}) #nolint
  exp$drop_response("profit")
  expect_length(exp$responses, 1)
  expect_error(exp$drop_response("jok"), regexp = "Response does not exist.")
  expect_error(exp$drop_response(12), regexp = "Response does not exist.")
  expect_error(exp$drop_response(c("ph", "profit")), regexp = "Only one response")
})

test_that("add_treatment", {
  exp = experiment$new(k = 3)
  exp$add_treatment()
  exp$add_treatment()
  expect_length(exp$treatments, 2)
  expect_error(exp$add_treatment(12), regexp = "Treatment name must be a single character vector")
  expect_error(exp$add_treatment(c("a", "b")), regexp = "Treatment name must be a single character vector")
  expect_error(exp$add_treatment(boundaries = 5), regexp = "Boundaries must be exactly two numeric values")
  expect_error(exp$add_treatment(boundaries = 1:3), regexp = "Boundaries must be exactly two numeric values")
  expect_error(exp$add_treatment(boundaries = letters[1:2]), regexp = "Boundaries must be exactly two numeric values")
  expect_error(exp$add_treatment("x1"), regexp = "A treatment with that name already exists")
  expect_error(exp$add_treatment("x2"), regexp = "A treatment with that name already exists")
  exp$add_treatment(boundaries = 1:2)
  expect_length(exp$treatments, 3)
  expect_error(exp$add_treatment(), "You have already added as many treatments as specified")
})

test_that("drop_treatment", {
  exp = experiment$new(k = 3)
  exp$add_treatment()
  exp$add_treatment()
  exp$add_treatment()
  exp$drop_treatment("x1")
  expect_length(exp$treatments, 2)
  expect_error(exp$drop_treatment("jok"), regexp = "Treatment does not exist.")
  expect_error(exp$drop_treatment(12), regexp = "Treatment does not exist.")
  expect_error(exp$drop_treatment(c("ph", "profit")), regexp = "Only one treatment")
})

test_that("add_constraint", {
  exp = experiment$new(k = 3)
  exp$add_treatment()
  exp$add_treatment()
  exp$add_treatment()
  expect_error(exp$add_constraint(x1 + x2), regexp = "Constraint must include a comparison operator")
  expect_error(exp$add_constraint("x1 + x2"), regexp = "Constraints must be passed as expressions.")
  expect_error(exp$add_constraint("x1 + x2 > 4"), regexp = "Constraints must be passed as expressions.")
  expect_error(exp$add_constraint(x1 + x4 > 4), regexp = "Invalid constraint provided.")
  exp$add_constraint(x1 + x2 > 4)
  exp$add_constraint(x1 + x3 < 7)
  expect_length(exp$constraints, 2)
})

context("test-setup_experiment")

test_that("add_response", {
  exp = experiment$new(k = 3)
  exp$add_response(name = "ph", range = c(0, 5), value = function(y, h){ (5 - y) ^ 2}) #nolint
  exp$add_response(name = "profit", value = function(y, h){1 / (max(h) - y + 0.0001)}) #nolint
  expect_length(exp$responses, 2)
})

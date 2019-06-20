context("test-execute_experiment")

test_that("submit_responses", {
  exp = experiment$new(k = 2)
  exp$add_treatment()
  exp$add_treatment()
  exp$add_response(name = "ph", range = c(0, 5), value = function(y, h){ (5 - y) ^ 2}) #nolint

  expect_error(exp$submit_responses(data = responses), regexp = "generate an initial")

  setup = data.frame(Trtmnt = c("x1", "x2"), Start = c(1, 1), step = c(2, 1))
  exp$generate_initial_simplex(method = "tilted", data = setup)

  responses = data.frame("Vertex_ID" = c(1, 2, 3), ph = c(5.1, 3.2, 1.7))
  expect_error(exp$submit_responses(data = responses), regexp = "not within stated boundaries")

  responses = data.frame(ph = c(5.1, 3.2, 1.7))
  expect_error(exp$submit_responses(data = responses), regexp = "data must have exactly")
  responses = data.frame("Vertex_ID" = c(1, 2, 3))
  expect_error(exp$submit_responses(data = responses), regexp = "data must have exactly")

  responses = data.frame("Vertex_ID" = c(1, 2, 3), ph = c(4.9, NA, 1.7))
  expect_error(exp$submit_responses(data = responses), regexp = "must have a numeric value")
  responses = data.frame("Vertex_ID" = c(1, 2, 3), ph = c(4.9, "3.2", 1.7))
  expect_error(exp$submit_responses(data = responses), regexp = "must have a numeric value")

  responses = data.frame("Vertex_ID" = c(1, 2, 3), ph = c(4.9, 3.2, 1.7))
  expect_message(exp$submit_responses(data = responses), regexp = "Responses recorded!")
  expect_error(exp$submit_responses(data = responses), regexp = "generate a new")
})

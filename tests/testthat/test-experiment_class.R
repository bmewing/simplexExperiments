test_that("class initializes", {
  expect_is(experiment$new(k = 3), "Simplex Experiment")
})

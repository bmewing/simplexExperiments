context("test-setup_experiment_utilities")

test_that("determine_cohypoplanarity", {
  expect_true(determine_cohypoplanarity(structure(c(10, 50, 40, 90, 50, 60),
                                                  .Dim = 3:2, .Dimnames = list(NULL, c("x1", "x2")))))
  expect_false(determine_cohypoplanarity(structure(c(60, 30, 65, 20, 40, 80),
                                                   .Dim = 3:2, .Dimnames = list(NULL, c("x1", "x2")))))
})

test_that("gen_fake_treatments", {
  expect_is(tmp <- gen_fake_treatments(c("x1", "x2")), "data.frame") #nolint
  expect_equal(names(tmp), c("x1", "x2"))
  expect_equal(nrow(tmp), 1)
  expect_true(all(tmp == 1))
})

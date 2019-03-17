context("test-misc")

test_that("nbinom test runs", {
  expect_s3_class(plot_nbinom(), c("gg", "ggplot"))
})

test_that("getter functions work", {
  expect_equal(class(migration_matrices()[[1]]), "matrix")
  expect_type(natural_mortality(), "double")
  expect_type(fishing_mortality(), "double")
  expect_type(weight_for_age(), "double")
})

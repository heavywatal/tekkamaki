test_that("nbinom test runs", {
  expect_s3_class(plot_nbinom(), c("gg", "ggplot"))
})

test_that("getter functions work", {
  expect_true(is.matrix(migration_matrices()[[1]]))
  expect_type(natural_mortality(), "double")
  expect_type(fishing_mortality(), "double")
  expect_type(weight_for_age(), "double")
})

test_that("nbinom test runs", {
  expect_s3_class(plot_nbinom(), c("gg", "ggplot"))
})

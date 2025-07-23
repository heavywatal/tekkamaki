test_that("augment/plot methods work", {
  set.seed(42L)
  result = tekka("-y40 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]

  df = expect_silent(augment(samples))
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_true(all(.colnames %in% names(df)))
  expect_identical(augment(samples, layout_demography), df)
  expect_error(augment(samples, 666), "Invalid type")

  p = expect_silent(plot(samples))
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_parameters_json() works", {
  p = expect_silent(plot_parameters_json())
  expect_s3_class(p, c("gg", "ggplot"))
})

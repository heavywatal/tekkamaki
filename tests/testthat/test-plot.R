test_that("augment/plot methods work", {
  result = tekka("--seed 42 -y40 -K100 -r2 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]

  expect_silent({
    df = augment(samples)
  })
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_true(all(.colnames %in% names(df)))
  expect_equal(augment(samples, layout_demography), df)
  expect_error(augment(samples, 666), "Invalid type")

  expect_silent({
    p = plot(samples)
  })
  expect_s3_class(p, c("gg", "ggplot"))
})

context("test-plot")

test_that("augment/plot methods work", {
  result = tekka("--seed 42 -n80 -y40 -K80 -r1 -l4 --sa 8,8 --sj 8,8")
  samples = result$sample_family[[1L]]
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_named(augment(samples), .colnames)
  expect_silent({
    p = plot(samples)
  })
  expect_s3_class(p, c("gg", "ggplot"))
})

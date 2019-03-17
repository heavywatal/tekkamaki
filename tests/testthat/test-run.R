context("test-run")

redirect = function(expr, type = c("message", "output"), file = "/dev/null") {
  type = match.arg(type)
  on.exit(sink(NULL, type = type))
  sink(file, type = type)
  invisible(eval(expr))
}

test_that("tekka runs", {
  expect_silent({result = tekka()})
  expect_s3_class(result, "data.frame")
  expect_s3_class(result$sample_family[[1L]], "data.frame")
  expect_s3_class(result$demography[[1L]], "data.frame")
  expect_output(redirect(tekka("-h")), "Usage: tekka")
})

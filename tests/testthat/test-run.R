test_that("tekka runs", {
  expect_silent({
    result = tekka()
  })
  expect_s3_class(result, "data.frame")
  expect_s3_class(result$sample_family[[1L]], "data.frame")
  expect_s3_class(result$demography[[1L]], "data.frame")
  expect_output(capture.output(tekka("-h"), type = "message"), "Usage: tekka")
})

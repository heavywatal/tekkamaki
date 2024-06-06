test_that("tekka is installed", {
  expect_true(file.exists(tekka_path()))
  expect_vector(tekka_version(), ptype = character(), size = 1L)
  expect_silent(zzz_do_nothing())
})

test_that("tekka runs", {
  expect_message(tekka("--help"), "Usage")
  expect_silent({
    result = tekka()
  })
  expect_s3_class(result, "data.frame")
  expect_s3_class(result$sample_family[[1L]], "data.frame")
  expect_s3_class(result$demography[[1L]], "data.frame")
})

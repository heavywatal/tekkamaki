test_that("tekka is installed", {
  expect_true(file.exists(tekka_path()))
  expect_vector(tekka_version(), ptype = character(), size = 1L)
  expect_silent(zzz_do_nothing())
})

test_that("tekka runs", {
  expect_message(tekka("--help"), "Usage")
  expect_message(tekka("--version"), "v")
  expect_silent({
    result = tekka()
  })
  expect_s3_class(result, "data.frame")
  sample_family = result$sample_family[[1L]]
  expect_s3_class(sample_family, "data.frame")
  sample_family |>
    dplyr::select(dplyr::matches("(?:^|_)id$")) |>
    duplicated() |>
    any() |>
    expect_false()
  expect_s3_class(result$demography[[1L]], "data.frame")
})

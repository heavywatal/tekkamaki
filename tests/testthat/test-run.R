test_that("tekka is installed", {
  expect_true(file.exists(tekka_path()))
})

test_that("tekka runs", {
  result = expect_silent(tekka())
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

test_that("help and version can be displayed", {
  tekka_version() |>
    expect_silent() |>
    expect_type("character") |>
    expect_length(1L) |>
    expect_match("^v?\\d+\\.\\d+([-.]\\d+)?(-\\d+)?(-g[a-fA-F0-9]{7,40})?(-dirty)?$")
  expect_message(tekka("--help"), "Usage")
})

test_that("sanitize_cache_dir works", {
  default = tempdir()
  expect_identical(sanitize_cache_dir(""), default)
  expect_identical(sanitize_cache_dir(character(0)), default)
  expect_identical(sanitize_cache_dir(NA_character_), default)
  expect_identical(sanitize_cache_dir(NA), default)
  expect_identical(sanitize_cache_dir(NULL), default)
  expect_identical(sanitize_cache_dir(FALSE), default)
  expect_identical(sanitize_cache_dir(TRUE), ".")
  expect_identical(sanitize_cache_dir("path"), "path")
})

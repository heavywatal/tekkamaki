test_that("Graph functions works", {
  result = tekka("--seed 42 -y40 -K100 -r2 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  expect_silent({
    kinship = find_kinship(samples)
  })
  expect_s3_class(kinship, "data.frame")
  expect_silent({
    kinship = find_kinship(samples, experimental = TRUE)
  })
  expect_s3_class(kinship, "data.frame")
  result = tekka("--seed 42 -y10 -K100 -r1 -l1 --sa 0,0 --sj 0,0")
  samples = result$sample_family[[1L]]
  expect_message(find_kinship(samples), "No kinship found")
})

test_that("POP and HSP work", {
  result = tekka("--seed 42 -y40 -K100 -r2 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  expect_silent({
    hsp = as_hsp(samples)
  })
  as_pop(samples)
  expect_silent({
    pop = as_pop(samples)
  })
  expect_s3_class(hsp, c("hsp", "data.frame"))
  expect_s3_class(pop, c("pop", "data.frame"))
  path = tempfile(fileext = ".tsv")
  expect_silent(write_hsp(hsp, path))
  expect_identical(read_hsp(path), hsp)
  expect_error(write_hsp(pop, path))
  expect_silent(write_pop(pop, path))
  expect_identical(read_pop(path), pop)
  expect_error(write_pop(hsp, path))

  kinship = find_kinship(samples)
  kinship |>
    dplyr::filter(.data$label == "HS") |>
    nrow() |>
    expect_identical(sum(hsp$hsps))
})

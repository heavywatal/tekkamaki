test_that("Graph functions works", {
  set.seed(42L)
  result = tekka("-y40 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  expect_silent({
    kinship = find_kinship(samples)
  })
  expect_s3_class(kinship, "data.frame")
  expect_silent({
    kinship = find_kinship(samples, experimental = TRUE)
  })
  expect_s3_class(kinship, "data.frame")
  result = tekka("-y10 -l1 --sa 0,0 --sj 0,0")
  samples = result$sample_family[[1L]]
  expect_message(find_kinship(samples), "No kinship found")
})

test_that("POP and HSP work", {
  set.seed(42L)
  result = tekka("-R1000 -S0 -y40 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  captured = samples |> dplyr::filter(!is.na(.data$capture_year))
  adults = captured |> dplyr::filter(birth_year < capture_year)
  nsam = nrow(captured)
  nad = nrow(adults)
  expect_silent({
    hsp = as_hsp(samples)
  })
  expect_identical(sum(hsp$comps), choose2(nsam))
  expect_silent({
    pop = as_pop(samples)
  })
  expect_identical(sum(pop$comps), nsam * nad - nad)
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
  kcount = kinship |>
    dplyr::count(label) |>
    tidyr::complete(label, fill = list(n = 0L)) |>
    tibble::deframe()
  # TODO: as_hsp() counts a FS pair as two HS pairs
  expect_identical(sum(hsp$hsps), unname(kcount["HS"] + 2L * kcount["FS"]))
  expect_identical(sum(pop$pops), unname(kcount["PO"]))
})

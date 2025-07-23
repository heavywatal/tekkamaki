test_that("Graph functions works", {
  set.seed(42L)
  result = tekka("-R4000 -S0 -y40 -l4 --sa 4,4 --sj 4,4")
  samples = result$sample_family[[1L]]
  kinship = expect_silent(find_kinship(samples))
  expect_s3_class(kinship, "data.frame")
  kinship = expect_silent(find_kinship(samples, experimental = TRUE))
  expect_s3_class(kinship, "data.frame")
  result = tekka("-y10 -l1 --sa 0,0 --sj 0,0")
  samples = result$sample_family[[1L]]
  expect_message(find_kinship(samples), "No kinship found")
})

test_that("POP and HSP work", {
  set.seed(42L)
  result = tekka("-R4000 -S0 -y40 -l4 --sa 4,4 --sj 4,4")
  samples = result$sample_family[[1L]]
  captured = samples |>
    dplyr::filter(!is.na(.data$capture_year)) |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::rename(cohort = "birth_year")
  nsam = nrow(captured)
  hsp = expect_silent(as_hsp(samples))
  hsp2 = expect_silent(as_hsp2(samples))
  expect_identical(sum(hsp$comps), choose2(nsam))
  expect_identical(sum(hsp2$comps), choose2(nsam))
  pop = expect_silent(as_pop(samples))
  pop2 = expect_silent(as_pop2(samples))
  cohort_n = dplyr::count(captured, .data$cohort)$n
  sum_pop_comps = choose2(nsam) - sum(choose2(cohort_n))
  expect_identical(sum(pop$comps), sum_pop_comps)
  expect_identical(sum(pop2$comps), sum_pop_comps)
  fsp = captured |>
    filter_sibs() |>
    group_by_fsp() |>
    count_coh_loc(name = "fsps")

  expect_s3_class(hsp, c("hsp", "data.frame"))
  expect_s3_class(pop, c("pop", "data.frame"))
  expect_s3_class(hsp2, c("hsp2", "data.frame"))
  expect_s3_class(pop2, c("pop2", "data.frame"))
  path = tempfile(fileext = ".tsv")
  expect_silent(write_hsp(hsp, path))
  expect_identical(read_hsp(path), hsp)
  expect_silent(write_hsp2(hsp2, path))
  expect_identical(read_hsp2(path), hsp2)
  expect_silent(write_pop(pop, path))
  expect_identical(read_pop(path), pop)
  expect_silent(write_pop2(pop2, path))
  expect_identical(read_pop2(path), pop2)
  expect_error(write_hsp(pop, path))
  expect_error(write_pop(hsp, path))
  expect_error(write_hsp2(pop, path))
  expect_error(write_pop2(hsp, path))

  kinship = find_kinship(samples)
  kcount = kinship |>
    dplyr::count(label) |>
    tidyr::complete(label, fill = list(n = 0L)) |>
    tibble::deframe()
  expect_identical(sum(hsp$hsps), kcount[["HS"]])
  expect_identical(sum(hsp2$hsps), kcount[["HS"]])
  expect_identical(sum(fsp$fsps), kcount[["FS"]])
  expect_identical(sum(pop$pops), kcount[["PO"]])
  expect_identical(sum(pop2$pops), kcount[["PO"]])
})

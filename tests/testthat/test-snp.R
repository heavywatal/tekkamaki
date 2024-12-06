test_that("genealogy methods and make_snp() work", {
  set.seed(42L)
  result = tekka("-y40 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  segments = gather_segments(samples) |>
    expect_silent()
  genealogy = make_gene_genealogy(segments) |>
    expect_silent()
  expect_type(count_uncoalesced(genealogy), "integer")
  df = augment(genealogy) |>
    expect_silent()
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_true(all(.colnames %in% names(df)))
  p = plot(genealogy) |>
    expect_silent() |>
    expect_s3_class(c("gg", "ggplot"))

  nsam = sum(!is.na(samples$capture_year))
  ss = c(2L, 3L)
  m = make_snp_chromosome(genealogy, ss[1L]) |>
    expect_silent()
  expect_identical(dim(m), c(2L * nsam, ss[1L]))
  l = make_snp(samples, ss = ss) |>
    expect_silent() |>
    expect_length(length(ss))
})

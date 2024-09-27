test_that("genealogy methods and make_snp() work", {
  result = tekka("--seed 42 -y40 -K100 -r2 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  expect_silent({
    segments = gather_segments(samples)
  })
  expect_silent({
    genealogy = make_gene_genealogy(segments)
  })
  expect_type(count_uncoalesced(genealogy), "integer")
  expect_silent({
    df = augment(genealogy)
  })
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_true(all(.colnames %in% names(df)))
  expect_silent({
    p = plot(genealogy)
  })
  expect_s3_class(p, c("gg", "ggplot"))

  nsam = sum(!is.na(samples$capture_year))
  ss = c(2L, 3L)
  expect_silent({
    m = make_snp(samples, ss = ss)
  })
})

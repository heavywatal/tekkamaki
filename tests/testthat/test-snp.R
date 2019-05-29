test_that("genealogy methods work", {
  result = tekka("--seed 42 -n80 -y40 -K90 -r1 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  expect_silent({
    chromosomes = gather_chromosome(samples)
  })
  expect_silent({
    genealogy = make_gene_genealogy(chromosomes)
  })
  expect_silent({
    df = augment(genealogy)
  })
  .colnames = c("from", "to", "x", "y", "xend", "yend", "label", "sampled")
  expect_true(all(.colnames %in% names(df)))
  expect_silent({
    p = plot(genealogy)
  })
  expect_s3_class(p, c("gg", "ggplot"))
})

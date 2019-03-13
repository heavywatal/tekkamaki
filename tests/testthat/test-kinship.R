context("test-kinship")

test_that("Graph functions works", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_s4_class(as_igraph(samples), "Rcpp_IGraph")
  expect_s3_class(find_kinship(samples), "data.frame")
})

test_that("POP and HSP works", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_s3_class(as_hsp(samples), "data.frame")
  expect_s3_class(as_pop(samples), "data.frame")
})

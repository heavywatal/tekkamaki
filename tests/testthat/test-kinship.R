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
  expect_s3_class({hsp = as_hsp(samples)}, "data.frame")
  expect_s3_class({pop = as_pop(samples)}, "data.frame")
  path = tempfile()
  expect_silent(write_hsp(hsp, path))
  expect_error(write_hsp(pop, path))
  expect_silent(write_pop(pop, path))
  expect_error(write_pop(hsp, path))
})

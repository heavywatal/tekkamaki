context("test-kinship")

test_that("Graph functions works", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_s4_class(as_igraph(samples), "Rcpp_IGraph")
  expect_s3_class(find_kinship(samples), "data.frame")
})

test_that("POP and HSP work", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_silent({
    hsp = as_hsp(samples)
  })
  expect_silent({
    pop = as_pop(samples)
  })
  expect_s3_class(hsp, c("hsp", "data.frame"))
  expect_s3_class(pop, c("pop", "data.frame"))
  path = tempfile(fileext = ".tsv")
  expect_silent(write_hsp(hsp, path))
  expect_equal(read_hsp(path), hsp)
  expect_error(write_hsp(pop, path))
  expect_silent(write_pop(pop, path))
  expect_equal(read_pop(path), pop)
  expect_error(write_pop(hsp, path))
})

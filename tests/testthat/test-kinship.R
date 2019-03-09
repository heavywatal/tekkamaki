context("test-kinship")

test_that("Graph functions works", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_silent(g <- as_igraph(samples))
  expect_s3_class(g, "igraph")
  expect_silent(kinship <- find_kinship(samples))
  expect_s3_class(kinship, "data.frame")
})

test_that("POP and HSP works", {
  result = tekka()
  samples = result$sample_family[[1L]]
  expect_silent(hsp <- as_hsp(samples))
  expect_s3_class(hsp, "data.frame")
  expect_silent(pop <- as_pop(samples))
  expect_s3_class(pop, "data.frame")
})

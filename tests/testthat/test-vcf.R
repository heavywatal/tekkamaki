test_that("vcf works", {
  result = tekka("--seed 42 -y40 -K100 -r2 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  segments = gather_segments(samples)
  genealogy = make_gene_genealogy(segments)
  sample_size = 16L
  segsites = 3L
  snp = place_mutations(genealogy, segsites)
  expect_identical(dim(snp), c(2L * sample_size, segsites))
  vcf = as_vcf(snp) |>
    expect_s3_class("data.frame")
  expect_identical(dim(vcf), c(segsites, 9L + sample_size))
  file = tempfile(fileext = ".vcf")
  write_vcf(vcf, file)
  read_vcf(file) |>
    expect_identical(vcf)
  write_vcf(snp, file)
  read_vcf(file) |>
    expect_identical(vcf)
  lines = readr::read_lines(file)
  expect_true(stringr::str_starts(lines[1L], "##fileformat=VCF"))
  expect_true(stringr::str_starts(lines[2L], "#CHROM\tPOS\tID"))
})

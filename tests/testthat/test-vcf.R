test_that("vcf works", {
  set.seed(42L)
  result = tekka("-y40 -l2 --sa 2,2 --sj 2,2")
  samples = result$sample_family[[1L]]
  sample_size = 16L
  ss = c(2L, 1L)
  segsites = sum(ss)
  snp = make_snp(samples, ss)
  vcf_gt = as_vcf_gt(Reduce(cbind, snp))
  expect_identical(dim(vcf_gt), c(segsites, 1L))
  expect_identical(as_vcf_gt(vcf_gt), vcf_gt)
  vcf = as_vcf(snp) |>
    add_pos_id() |>
    expect_s3_class("data.frame")
  expect_identical(dim(vcf), c(segsites, 9L + 1L))
  expect_identical(as_vcf_gt(vcf), vcf_gt)
  vcf |> separate_gt() |> unite_gt() |>
    tibble::new_tibble(class = "vcf") |>
    expect_identical(vcf)
  file = tempfile(fileext = ".vcf")
  write_vcf(vcf, file)
  read_vcf(file) |>
    expect_identical(vcf)
  write_vcf(snp, file)
  read_vcf(file) |>
    as_vcf_gt() |>
    expect_identical(vcf_gt)
  lines = readr::read_lines(file)
  expect_true(stringr::str_starts(lines[1L], "##fileformat=VCF"))
  expect_true(stringr::str_starts(lines[2L], "#CHROM\tPOS\tID"))
})

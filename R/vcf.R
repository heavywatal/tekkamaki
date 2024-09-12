#' Utilities for VCF format
#'
#' [as_vcf()] converts a SNP matrix to a VCF-like data.frame.
#' @param x SNP matrix or VCF data.frame.
#' @param phased A logical to switch separators: `|` vs `/`.
#' @param chrom Characters.
#' @param pos Integers.
#' @rdname vcf
#' @export
as_vcf = function(x, phased = TRUE, chrom = NA_character_, pos = NA_integer_) {
  if (inherits(x, "vcf")) {
    return(x)
  }
  x |>
    as_vcf_genotype(phased = phased) |>
    dplyr::mutate(
      CHROM = chrom,
      POS = pos,
      ID = NA_character_,
      REF = NA_character_,
      ALT = NA_character_,
      QUAL = NA_character_,
      FILTER = NA_character_,
      INFO = NA_character_,
      FORMAT = "GT",
      .before = 1L
    ) |>
    tibble::new_tibble(class = "vcf")
}

#' @param file A path or connection.
#' @rdname vcf
#' @export
write_vcf = function(x, file) {
  x = as_vcf(x) |>
    dplyr::rename(`#CHROM` = "CHROM")
  meta = "##fileformat=VCFv4.5"
  readr::write_lines(meta, file)
  readr::write_tsv(x, file, na = ".", append = TRUE, col_names = TRUE)
}

#' @rdname vcf
#' @export
read_vcf = function(file) {
  .cols = readr::cols(POS = "i", .default = "c")
  readr::read_tsv(file, na = ".", comment = "##", col_types = .cols)[] |>
    dplyr::rename(CHROM = "#CHROM") |>
    tibble::new_tibble(class = "vcf")
}

as_vcf_genotype = function(x, phased = TRUE) {
  if (inherits(x, "vcf_gt")) {
    return(x)
  }
  if (inherits(x, "vcf")) {
    x = x |>
      dplyr::select(!c("CHROM", "POS", "ID", "REF", "ALT")) |>
      dplyr::select(!c("QUAL", "FILTER", "INFO", "FORMAT"))
  } else {
    stopifnot(is.matrix(x))
    sep = ifelse(phased, "|", "/")
    is_mother = stringr::str_ends(rownames(x), "-1")
    allele_mother = x[is_mother, ]
    allele_father = x[!is_mother, ]
    x = paste(allele_mother, allele_father, sep = sep)
    dim(x) = dim(allele_mother)
    rownames(x) = stringr::str_remove(rownames(allele_mother), "-\\d$")
    x = as.data.frame(t(x))
  }
  tibble::new_tibble(x, class = "vcf_gt")
}

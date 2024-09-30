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
  UseMethod("as_vcf", x)
}

#' @export
as_vcf.default = function(x, phased = TRUE, chrom = NA_character_, pos = NA_integer_) {
  x |>
    as_vcf_gt(phased = phased) |>
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

#' @export
as_vcf.list = function(x, phased = TRUE, ...) {
  purrr::imap(x, \(.x, .i) {
    as_vcf(.x, phased = phased, chrom = as.character(.i))
  }) |> purrr::list_rbind()
}

#' @export
as_vcf.vcf = function(x, ...) x

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

#' @rdname vcf
#' @export
as_vcf_gt = function(x, phased = TRUE) UseMethod("as_vcf_gt", x)

#' @export
as_vcf_gt.default = function(x, phased = TRUE) {
  stopifnot(is.matrix(x))
  sep = ifelse(phased, "|", "/")
  is_mother = stringr::str_ends(rownames(x), "-1")
  allele_mother = x[is_mother, , drop = FALSE]
  allele_father = x[!is_mother, , drop = FALSE]
  x = paste(allele_mother, allele_father, sep = sep)
  dim(x) = dim(allele_mother)
  rownames(x) = stringr::str_remove(rownames(allele_mother), "-\\d$")
  x = as.data.frame(t(x))
  tibble::new_tibble(x, class = "vcf_gt")
}

#' @export
as_vcf_gt.vcf_gt = function(x, ...) x

#' @export
as_vcf_gt.vcf = function(x, ...) {
  tibble::new_tibble(x, class = "vcf_gt") |>
    dplyr::select(!c("CHROM", "POS", "ID", "REF", "ALT")) |>
    dplyr::select(!c("QUAL", "FILTER", "INFO", "FORMAT"))
}

#' @rdname vcf
#' @export
add_pos_id = function(x) {
  stopifnot(inherits(x, "vcf"))
  x |>
    dplyr::mutate(POS = dplyr::row_number(), .by = "CHROM") |>
    dplyr::mutate(ID = paste(.data$CHROM, .data$POS, sep = "-"))
}

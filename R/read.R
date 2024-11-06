#' Read functions
#'
#' @details
#' `read_result` reads a result from tekka.
#' @inheritParams readr::read_tsv
#' @rdname read
#' @export
read_result = function(file) {
  df = readr::read_tsv(file, col_types = readr::cols(.default = "i"), show_col_types = FALSE)
  class(df) = c("sample_family", "tbl_df", "tbl", "data.frame")
  df
}

.read_result = function(indir = getwd()) {
  res = .read_conf(indir)
  sample_family = file.path(indir, "sample_family.tsv.gz") |>
    read_result()
  demography = file.path(indir, "demography.tsv.gz") |>
    readr::read_tsv(show_col_types = FALSE)
  res[["sample_family"]] = list(sample_family)
  res[["demography"]] = list(demography)
  res
}

.read_conf = function(indir = getwd()) {
  json = file.path(indir, "config.json")
  obj = jsonlite::read_json(json, simplifyVector = TRUE)
  obj[["sample_size_adult"]] = list(obj[["sample_size_adult"]])
  obj[["sample_size_juvenile"]] = list(obj[["sample_size_juvenile"]])
  tibble::new_tibble(obj)
}

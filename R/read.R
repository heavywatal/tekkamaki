#' Read results from `tekka`
#'
#' The `tekka` simulator writes results and configuration files to a directory.
#' This function reads them into a one-row data.frame.
#' @param indir An output directory of `tekka`.
#' @returns A one-row data.frame with the configuration parameters and results.
#' There are two result columns:
#' - `sample_family`: a list of data.frames with samples and their ancestors.
#' - `demography`: a list of data.frames with population demography.
#' @seealso [tekka()] for example usage.
#' @rdname read
#' @export
read_tekka = function(indir = getwd()) {
  res = read_conf(indir)
  sample_family = file.path(indir, "sample_family.tsv.gz") |>
    read_sample_family()
  demography = file.path(indir, "demography.tsv.gz") |>
    read_tsv_int()
  res[["sample_family"]] = list(sample_family)
  res[["demography"]] = list(demography[])
  res
}

read_sample_family = function(file) {
  x = read_tsv_int(file)
  class(x) = c("sample_family", class(x))
  x
}

read_tsv_int = function(file, ...) {
  readr::read_tsv(file, col_types = readr::cols(.default = "i"), show_col_types = FALSE, ...)[]
}

read_conf = function(indir = getwd()) {
  json = file.path(indir, "config.json")
  obj = jsonlite::read_json(json, simplifyVector = TRUE)
  obj[["sample_size_adult"]] = list(obj[["sample_size_adult"]])
  obj[["sample_size_juvenile"]] = list(obj[["sample_size_juvenile"]])
  tibble::new_tibble(obj)
}

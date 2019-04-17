#' Read functions
#'
#' @details
#' `read_result` reads a result from tekka.
#' @inheritParams readr::read_tsv
#' @rdname read
#' @export
read_result = function(file) {
  df = readr::read_tsv(file, col_types = readr::cols(.default = "i"))
  class(df) = c("sample_family", "tbl_df", "tbl", "data.frame")
  df
}

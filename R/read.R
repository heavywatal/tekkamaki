#' Read functions
#'
#' @details
#' `read_result` reads a result from tekka.
#' @inheritParams readr::read_tsv
#' @rdname read
#' @export
read_result = function(file) {
  readr::read_tsv(file, col_types = readr::cols(.default = "i"))
}

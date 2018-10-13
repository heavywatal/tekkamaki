#' Read functions
#'
#' @description
#' `read_result` reads a result from blackthunnus.
#' @inheritParams readr::read_tsv
#' @rdname read
#' @export
read_result = function(file) {
  readr::read_tsv(file, col_types = readr::cols(
    id = "c",
    father_id = "c",
    mother_id = "c",
    capture_year = "i"
  ))
}

#' Get the path to the tekka installation.
#'
#' @rdname path
#' @export
tekka_path = function() {
  scan(system.file("path", package = "tekkamaki"), what = character(0), quiet = TRUE)
}

#' @rdname path
#' @export
tekka_version = function() {
  system2(tekka_path(), "--version", stdout = TRUE, stderr = FALSE)
}

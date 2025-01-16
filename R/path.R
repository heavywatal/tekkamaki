#' Get the path to the tekka installation.
#'
#' @rdname path
#' @export
tekka_path = function() {
  x = tekka_path_file()
  if (!file.exists(x)) {
    x = tekka_path_exec()
    if (!file.exists(x)) x = "tekka"
  }
  x
}

tekka_path_file = function() {
  file = system.file("path", package = "tekkamaki")
  scan(file, what = character(0), quiet = TRUE)
}

tekka_path_exec = function() {
  system.file("exec", "tekka", package = "tekkamaki")
}

#' @rdname path
#' @export
tekka_version = function() {
  tekka("--version")
}

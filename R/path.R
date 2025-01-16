#' Get the path to the tekka installation.
#'
#' @rdname path
#' @export
tekka_path = function() {
  x = tekka_path_config()
  if (!file.exists(x)) {
    x = tekka_path_exec()
    if (!file.exists(x)) x = "tekka"
  }
  x
}

tekka_path_exec = function() {
  system.file("exec", "tekka", package = "tekkamaki")
}

#' @rdname path
#' @export
tekka_version = function() {
  tekka("--version")
}

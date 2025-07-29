#' Get information about the `tekka` executable
#'
#' The simulator `tekka` is a command-line tool written in C++.
#' The executable is built and installed along with this R package
#' unless the pre-installed one was found during the package installation.
#' @returns `tekka_path()` returns the path to the `tekka` executable.
#' @seealso [tekka()] to run the simulator.
#' @rdname path
#' @export
#' @examples
#' \dontrun{
#' tekka_path()
#'
#' tekka_version()
#' }
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

#' @returns `tekka_version()` returns the version of the `tekka` executable.
#' @rdname path
#' @export
tekka_version = function() {
  tekka("--version")
}

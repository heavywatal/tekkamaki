#' Run C++ simulation.
#'
#' A result is first written to a directory in `cache`,
#' and then read into a data.frame.
#' If a previous result with exactly the same arguments is found in `cache`,
#' it is read without calling `tekka`.
#' @param args Command line arguments as a string vector.
#' See [`tekka` manual](https://heavywatal.github.io/tekka/group__parameters.html)
#' and `tekka("--help")` for available options.
#' Note that a random `--seed` is appended if not given.
#' Use `set.seed()` or `--seed` explicitly for reproducibility and caching.
#' @param cache Parent directory for tekka output.
#' [tempdir()] is used by default (`FALSE`),
#' which is discarded at the end of an R session.
#' `TRUE` is equivalent to "." (current directory).
#' @rdname tekka
#' @export
tekka = function(args = character(0L), cache = FALSE) {
  if (length(args) == 1L) {
    args = stringr::str_split_1(args, "\\s+")
  }
  if ("--version" %in% args) {
    return(system2(tekka_path(), "--version", stdout = TRUE))
  }
  if (any(c("-h", "--help", "--default") %in% args)) {
    msg = system2(tekka_path(), args, stdout = TRUE)
    return(invisible(message(paste0(msg, collapse = "\n"))))
  }
  args = append_seed(args)
  if (isFALSE(cache)) {
    cache = tempdir()
  } else if (isTRUE(cache)) {
    cache = "."
  }
  outdir = file.path(cache, cache_name(args))
  if (!dir.exists(outdir)) {
    ret = system2(tekka_path(), c(args, "-o", outdir))
    stopifnot(ret == 0L)
  }
  .read_result(outdir)
}

cache_name = function(args) {
  x = stringr::str_flatten(args) |>
    stringr::str_remove_all("[ =.]+") |>
    stringr::str_replace_all("-+", "-")
  paste0(".tekka", x)
}

append_seed = function(args, seed = NULL) {
  if (!any(stringr::str_starts(args, "--seed"))) {
    args = c(args, paste0("--seed=", seed %||% runif.int(1L)))
  }
  args
}

runif.int = function(n, min = -.Machine$integer.max, max = .Machine$integer.max) {
  offset = min - 1
  as.integer(sample.int(max - offset, n, replace = TRUE) + offset)
}

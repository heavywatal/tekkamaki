#' Run the `tekka` simulator.
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
#' `TRUE` is equivalent to "." (current directory).
#' [tempdir()] is used in other cases including the default (`NULL`),
#' which is discarded at the end of an R session.
#' Set `FALSE` to force `tekka` to run and overwrite previous results if any.
#' @returns A data.frame read by [read_tekka()].
#' @seealso [tekka_path()] to get information about the `tekka` installation.
#' @rdname tekka
#' @export
#' @examples
#' set.seed(666)
#' # tekka("--help")
#'
#' result = tekka("-y25 -l2 --sa 2,2 --sj 2,2")
#' result |> dplyr::select(!"outdir")
#'
#' result$sample_family[[1L]]
#'
#' result$demography[[1L]]
tekka = function(args = character(0L), cache = NULL) {
  if (length(args) == 1L) {
    args = stringr::str_split_1(args, "\\s+")
  }
  if (any(c("--version", "--json") %in% args)) {
    return(system2(tekka_path(), args, stdout = TRUE))
  }
  if (any(c("-h", "--help", "--default") %in% args)) {
    msg = system2(tekka_path(), args, stdout = TRUE)
    return(invisible(message(paste(msg, collapse = "\n"))))
  }
  args = append_seed(args)
  cache_dir = sanitize_cache_dir(cache)
  outdir = file.path(cache_dir, cache_name(args))
  if (isFALSE(cache) || !dir.exists(outdir)) {
    ret = system2(tekka_path(), c(args, "-o", outdir))
    stopifnot(ret == 0L)
  }
  read_tekka(outdir)
}

sanitize_cache_dir = function(cache) {
  if (isTRUE(cache)) {
    cache = "."
  } else if (length(cache) != 1L || !is.character(cache) || is.na(cache) || !nzchar(cache)) {
    cache = tempdir()
  }
  cache
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
  .offset = min - 1
  as.integer(sample.int(max - .offset, n, replace = TRUE) + .offset)
}

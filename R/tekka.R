#' Run C++ simulation.
#' @param args command line arguments as a string vector.
#' @param cache use cache if TRUE.
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
  cache_dir = cache_name(args)
  if (!cache) {
    cache_dir = file.path(tempdir(), cache_dir)
  }
  if (!dir.exists(cache_dir)) {
    ret = system2(tekka_path(), c(args, "-o", cache_dir))
    stopifnot(ret == 0L)
  }
  .read_result(cache_dir)
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

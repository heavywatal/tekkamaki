#' POP format
#'
#' @details
#' `as_pop` converts a result data frame to POP format.
#' @param .tbl sample_family
#' @param min_adult_age integer
#' @rdname pop
#' @export
as_pop = function(.tbl, min_adult_age = 5L) {
  .tbl = .tbl |>
    pairwise_parent_offspring(min_adult_age) |>
    summarize_pop()
  class(.tbl) = c(class(.tbl), "pop")
  .tbl
}

#' @details
#' `write_pop` writes a POP data frame to a file.
#' @param x An outcome of [as_pop()]
#' @param path A file name or connection to write to
#' @rdname pop
#' @export
write_pop = function(x, path = "pop.txt") {
  stopifnot(inherits(x, "pop"))
  readr::write_file("# ckdat:\n", path)
  readr::write_tsv(x, path, na = "", append = TRUE, col_names = FALSE)
}

#' @rdname pop
#' @export
read_pop = function(path) {
  x = readr::read_tsv(path,
    col_names = c("cohort", "capture_year", "capture_age", "location", "pops", "comps"),
    col_types = "iiiiii", comment = "#", show_col_types = FALSE
  )
  class(x) = c(class(x), "pop")
  x
}

pairwise_parent_offspring = function(.tbl, min_adult_age) {
  .tbl = dplyr::filter(.tbl, !is.na(.data$capture_year))
  adults = .tbl |>
    dplyr::filter(.data$capture_year - .data$birth_year >= min_adult_age) |>
    dplyr::transmute(.data$id, .data$capture_year, adult_birth_year = .data$birth_year, .data$location)
  juveniles = .tbl |>
    dplyr::transmute(.data$id, cohort = .data$birth_year, .data$mother_id, .data$father_id)
  tidyr::crossing(id = .tbl$id, adult_id = adults$id) |>
    dplyr::filter(.data$id > .data$adult_id) |>
    dplyr::left_join(juveniles, by = "id") |>
    dplyr::left_join(adults, by = c(adult_id = "id")) |>
    dplyr::filter(.data$cohort - .data$adult_birth_year >= min_adult_age) |>
    dplyr::mutate(is_pop = (.data$adult_id == .data$mother_id | .data$adult_id == .data$father_id))
}

summarize_pop = function(.tbl) {
  n = dplyr::n
  .tbl |>
    dplyr::mutate(capture_age = .data$capture_year - .data$adult_birth_year) |>
    dplyr::group_by(.data$cohort, .data$capture_year, .data$capture_age, .data$location) |>
    dplyr::summarise(pops = sum(!!as.name("is_pop")), comps = n()) |>
    dplyr::ungroup() |>
    tidyr::complete(.data$cohort, .data$capture_year, .data$capture_age, fill = list(pops = 0L, comps = 0L))
}

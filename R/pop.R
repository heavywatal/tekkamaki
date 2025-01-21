#' POP format
#'
#' @details
#' `as_pop` converts a result data frame to POP format.
#' @param samples sample_family
#' @param min_adult_age integer
#' @rdname pop
#' @export
as_pop = function(samples, min_adult_age = 4L) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year))
  adults = captured |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::filter(.data$capture_age >= min_adult_age) |>
    dplyr::select("id", "capture_year", "capture_age", "location")
  relation = captured |>
    dplyr::select("id", "mother_id", "father_id", cohort = "birth_year")
  comps = count_pop_comps(adults, relation)
  pop = count_pops(adults, relation) |>
    dplyr::right_join(comps, by = c("cohort", "capture_year", "capture_age", "location")) |>
    tidyr::replace_na(list(pops = 0L)) |>
    bloat_pop()
  class(pop) = c("pop", class(pop))
  pop
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
  class(x) = c("pop", class(x))
  x
}

count_pops = function(adults, relation) {
  parents = adults |>
    dplyr::filter(.data$id %in% relation$father_id | .data$id %in% relation$mother_id)
  offspring_with_mother = relation |>
    dplyr::select(id = "mother_id", "cohort") |>
    dplyr::filter(.data$id %in% parents$id)
  offspring_with_father = relation |>
    dplyr::select(id = "father_id", "cohort") |>
    dplyr::filter(.data$id %in% parents$id)
  dplyr::bind_rows(offspring_with_mother, offspring_with_father) |>
    dplyr::left_join(parents, by = "id") |>
    dplyr::count(.data$cohort, .data$capture_year, .data$capture_age, .data$location, name = "pops")
}

count_pop_comps = function(adults, relation) {
  cnt_adults = adults |>
    dplyr::count(.data$capture_year, .data$capture_age, .data$location)
  cnt_cohort = relation |>
    dplyr::count(.data$cohort)
  cnt_cohort |>
    dplyr::mutate(data = lapply(.data$n, \(.x) {
      dplyr::mutate(cnt_adults, n = .data$n * .x)
    }), n = NULL) |>
    tidyr::unnest("data") |>
    dplyr::rename(comps = "n")
}

bloat_pop = function(pop) {
  pop |>
    tidyr::complete(.data$cohort, .data$capture_year, .data$capture_age, fill = list(pops = 0L, comps = 0L))
}

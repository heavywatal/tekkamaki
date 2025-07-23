#' POP format
#'
#' Parent-offspring pairs are counted between potential offspring cohort and
#' potential parents grouped by age, year, and location of sampling.
#' Adults are included in potential offspring.
#' @details
#' [as_pop()] converts a `sample_family` data frame to POP format.
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @return A data.frame with "pop" class and six columns:
#' - `cohort`: birth year of offspring
#' - `capture_year`: of parents
#' - `capture_age`: of parents
#' - `location`: where parents were sampled
#' - `pops`: the number of parent-offspring pairs
#' - `comps`: the number of possible comparisons
#' @rdname pop
#' @export
as_pop = function(samples) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year)) |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::rename(cohort = "birth_year")
  comps = count_pop_comps(captured)
  pop = count_pops(captured) |>
    dplyr::right_join(comps, by = pop_keys) |>
    tidyr::replace_na(list(pops = 0L)) |>
    bloat_pop()
  class(pop) = c("pop", class(pop))
  pop
}

#' @details
#' [write_pop()] writes a POP data.frame to a file.
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
    col_names = c(pop_keys, "pops", "comps"),
    col_types = "iiiiii", comment = "#", show_col_types = FALSE
  )[]
  class(x) = c("pop", class(x))
  x
}

pop_keys = c("cohort", "capture_year", "capture_age", "location")

count_pops = function(captured) {
  parents = captured |>
    dplyr::select(!"cohort") |>
    dplyr::filter(.data$id %in% captured$father_id | .data$id %in% captured$mother_id)
  offspring_with_mother = captured |>
    dplyr::select(id = "mother_id", "cohort") |>
    dplyr::filter(.data$id %in% parents$id)
  offspring_with_father = captured |>
    dplyr::select(id = "father_id", "cohort") |>
    dplyr::filter(.data$id %in% parents$id)
  dplyr::bind_rows(offspring_with_mother, offspring_with_father) |>
    dplyr::left_join(parents, by = "id") |>
    dplyr::count(.data$cohort, .data$capture_year, .data$capture_age, .data$location, name = "pops")
}

count_pop_comps = function(captured) {
  cnt_adults = captured |>
    dplyr::count(.data$capture_year, .data$capture_age, .data$location, name = "n_i")
  captured |>
    dplyr::count(.data$cohort, name = "n_j") |>
    dplyr::mutate(nested = purrr::map(.data$cohort, \(x) {
      dplyr::filter(cnt_adults, x > .data$capture_year - .data$capture_age)
    })) |>
    tidyr::unnest("nested") |>
    dplyr::mutate(comps = .data$n_i * .data$n_j) |>
    dplyr::select(dplyr::all_of(pop_keys), "comps")
}

bloat_pop = function(pop) {
  pop |>
    tidyr::complete(.data$cohort, .data$capture_year, .data$capture_age, fill = list(pops = 0L, comps = 0L))
}

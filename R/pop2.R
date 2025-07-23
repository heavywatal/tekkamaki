#' Extended POP format
#'
#' Parent-offspring pairs are counted between potential offspring cohort and
#' potential parents grouped by birth year, capture age, location of sampling.
#' Adults are included in potential offspring.
#' @details
#' [as_pop2()] converts a `sample_family` data frame to POP format.
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @return A data.frame with "pop" class and six columns:
#' - `cohort_parent`, `cohort_offspring`: birth year of samples
#' - `capture_age_parent`, `capture_age_offspring`
#' - `location_parent`, `location_offspring`: of sampling
#' - `pops`: the number of parent-offspring pairs
#' - `comps`: the number of possible comparisons
#' @rdname pop2
#' @export
as_pop2 = function(samples) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year)) |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::rename(cohort = "birth_year") |>
    dplyr::select(!"capture_year")
  comps = count_pop2_comps(captured)
  pop = count_pops2(captured) |>
    dplyr::right_join(comps, by = pop2_keys) |>
    tidyr::replace_na(list(pops = 0L))
  tibble::new_tibble(pop, class = "pop2")
}

#' @details
#' [write_pop2()] writes a POP data.frame to a file.
#' @param x An outcome of [as_pop2()]
#' @param path A file name or connection to write to.
#' @rdname pop2
#' @export
write_pop2 = function(x, path = "pop2.tsv") {
  stopifnot(inherits(x, "pop2"))
  readr::write_tsv(x, path, na = "")
}

#' @rdname pop2
#' @export
read_pop2 = function(path) {
  x = readr::read_tsv(path, col_types = "iiiiiiii", comment = "#", show_col_types = FALSE)[]
  class(x) = c("pop2", class(x))
  x
}

pop2_keys = c(
  "cohort_parent", "cohort_offspring",
  "capture_age_parent", "capture_age_offspring",
  "location_parent", "location_offspring"
)

count_pops2 = function(captured) {
  parents = captured |>
    dplyr::select("id", "cohort", "capture_age", "location") |>
    dplyr::filter(.data$id %in% captured$father_id | .data$id %in% captured$mother_id) |>
    dplyr::rename_with(\(x) paste0(x, "_parent"), !"id")
  offspring_with_mother = captured |>
    dplyr::select(id = "mother_id", "cohort", "capture_age", "location") |>
    dplyr::filter(.data$id %in% parents$id)
  offspring_with_father = captured |>
    dplyr::select(id = "father_id", "cohort", "capture_age", "location") |>
    dplyr::filter(.data$id %in% parents$id)
  dplyr::bind_rows(offspring_with_mother, offspring_with_father) |>
    dplyr::rename_with(\(x) paste0(x, "_offspring"), !"id") |>
    dplyr::left_join(parents, by = "id") |>
    dplyr::count(!!!rlang::data_syms(pop2_keys), name = "pops")
}

count_pop2_comps = function(captured) {
  cnt = dplyr::count(captured, .data$cohort, .data$capture_age, .data$location)
  cnt_offspring = dplyr::rename_with(cnt, \(x) paste0(x, "_offspring"))
  cnt |>
    dplyr::rename_with(\(x) paste0(x, "_parent")) |>
    dplyr::mutate(nested = purrr::map(.data$cohort_parent, \(x) {
      dplyr::filter(cnt_offspring, .data$cohort_offspring > x)
    })) |>
    tidyr::unnest("nested") |>
    dplyr::mutate(comps = .data$n_parent * .data$n_offspring) |>
    dplyr::select(dplyr::all_of(pop2_keys), "comps")
}

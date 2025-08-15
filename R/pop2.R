#' Count parent-offspring pairs in POP2 format
#'
#' The extended version of POP format with additional columns for grouping.
#' @seealso [as_pop()] for the original POP format.
#' @seealso [find_kinship()] to count kinship within samples.
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @returns A data.frame of "pop2" subclass with the following columns:
#' - `cohort_parent`, `cohort_offspring`: birth year of samples
#' - `capture_age_parent`, `capture_age_offspring`
#' - `location_parent`, `location_offspring`: of sampling
#' - `pops`: the count of parent-offspring pairs observed within samples
#' - `comps`: the number of possible comparisons,
#'   including only pairs where one sample is younger than the other.
#' @rdname pop2
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' as_pop2(samples)
as_pop2 = function(samples) {
  captured = filter_sampled(samples)
  comps = count_pop2_comps(captured)
  pop = count_pops2(captured) |>
    dplyr::right_join(comps, by = pop2_keys) |>
    tidyr::replace_na(list(pops = 0L))
  tibble::new_tibble(pop, class = "pop2")
}

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

#' Count half-sib pairs in HSP2 format
#'
#' The extended version of HSP format with additional columns for grouping.
#' @seealso [as_hsp()] for the original HSP format.
#' @seealso [find_kinship()] to count kinship within samples.
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @returns A data.frame of "hsp2" subclass with the following columns:
#' - `cohort_i`, `cohort_j`: birth year of samples
#' - `capture_age_i`, `capture_age_j`: of samples
#' - `location_i`, `location_j`: of sampling
#' - `comps`: the number of possible comparisons between group *i* and *j*.
#'   All samples are compared pairwise, excluding self-comparisons.
#'   The total number of comparisons equals \\(n \\choose 2\\),
#'   where *n* is the total sample size.
#' - `hsps`: the count of half-sib pairs observed between group *i* and *j*
#' @rdname hsp2
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' as_hsp2(samples)
as_hsp2 = function(samples) {
  captured = filter_sampled(samples)
  comps = count_hsp2_comps(captured)
  count_hsp2(captured) |>
    dplyr::right_join(comps, by = hsp2_keys) |>
    dplyr::relocate("comps", .before = "hsps") |>
    tidyr::replace_na(list(hsps = 0L)) |>
    tibble::new_tibble(class = "hsp2")
}

#' @param x An outcome of [as_hsp2()].
#' @param path A file name or connection to write to.
#' @rdname hsp2
#' @export
write_hsp2 = function(x, path = "hsp2.tsv") {
  stopifnot(inherits(x, "hsp2"))
  readr::write_tsv(x, path, na = "")
}

#' @rdname hsp2
#' @export
read_hsp2 = function(path) {
  x = readr::read_tsv(path, col_types = "iiiiiiii", show_col_types = FALSE)[]
  class(x) = c("hsp2", class(x))
  x
}

hsp2_keys = c("cohort_i", "cohort_j", "capture_age_i", "capture_age_j", "location_i", "location_j")

count_hsp2 = function(captured) {
  sibs = filter_sibs(captured)
  fsp = group_by_fsp(sibs) |> count_coh_cap_loc(name = "fsps")
  hsp = group_by_hsp(sibs) |> count_coh_cap_loc(name = "hsps")
  dplyr::left_join(hsp, fsp, by = hsp2_keys) |>
    dplyr::mutate(hsps = .data$hsps - 2L * dplyr::coalesce(.data$fsps, 0L), fsps = NULL)
}

count_coh_cap_loc = function(x, name = NULL) {
  if (nrow(x) == 0L) {
    .names = c(hsp2_keys, name %||% "n")
    return(tibble::new_tibble(rep(list(integer(0L)), 7L), names = .names))
  }
  x |>
    dplyr::group_modify(\(g, ...) {
      fun = \(v) {
        data.frame(
          cohort_i = g$cohort[v[1L]],
          cohort_j = g$cohort[v[2L]],
          capture_age_i = g$capture_age[v[1L]],
          capture_age_j = g$capture_age[v[2L]],
          location_i = g$location[v[1L]],
          location_j = g$location[v[2L]]
        )
      }
      utils::combn(seq_len(nrow(g)), 2L, fun, simplify = FALSE) |>
        purrr::list_rbind()
    }) |>
    dplyr::ungroup() |>
    dplyr::count(
      .data$cohort_i, .data$cohort_j,
      .data$capture_age_i, .data$capture_age_j,
      .data$location_i, .data$location_j,
      name = name
    )
}

count_hsp2_comps = function(captured) {
  cnt = captured |>
    dplyr::count(.data$cohort, .data$capture_age, .data$location) |>
    tidyr::unite("coh_age_loc", "cohort", "capture_age", "location") |>
    dplyr::mutate(coh_age_loc = ordered(.data$coh_age_loc, levels = unique(.data$coh_age_loc)))
  tibble::tibble(
    df_i = cnt |> dplyr::rename_with(\(x) paste0(x, "_i")) |> list(),
    df_j = cnt |> dplyr::rename_with(\(x) paste0(x, "_j")) |> list()
  ) |>
    tidyr::unnest("df_i") |>
    tidyr::unnest("df_j") |>
    dplyr::filter(.data$coh_age_loc_i <= .data$coh_age_loc_j) |>
    dplyr::mutate(comps = ifelse(
      .data$coh_age_loc_i == .data$coh_age_loc_j, choose2(.data$n_i), .data$n_i * .data$n_j
    )) |>
    dplyr::select(!c("n_i", "n_j")) |>
    tidyr::separate("coh_age_loc_i", c("cohort_i", "capture_age_i", "location_i"), convert = TRUE) |>
    tidyr::separate("coh_age_loc_j", c("cohort_j", "capture_age_j", "location_j"), convert = TRUE)
}

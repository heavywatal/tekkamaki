#' Count half-sib pairs in HSP format
#'
#' Half-sib pairs are counted between samples grouped by birth year and
#' sampled location.
#' @seealso [as_hsp2()] for the extended HSP format.
#' @seealso [find_kinship()] to count kinship within samples.
#' @details
#' [as_hsp()] converts a result data frame to HSP format.
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @returns A data.frame of "hsp" subclass with six columns:
#' - `cohort_i`, `cohort_j`: birth year of samples
#' - `location_i`, `location_j`: of sampling
#' - `comps`: the number of possible comparisons between group *i* and *j*.
#'   All samples are compared pairwise, excluding self-comparisons.
#'   The total number of comparisons equals \\(n \\choose 2\\),
#'   where *n* is the total sample size.
#' - `hsps`: the count of half-sib pairs observed between group *i* and *j*
#' @rdname hsp
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' as_hsp(samples)
as_hsp = function(samples) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year)) |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::rename(cohort = "birth_year") |>
    dplyr::select(!"capture_year")
  comps = count_hsp_comps(captured)
  count_hsp(captured) |>
    dplyr::right_join(comps, by = hsp_keys) |>
    dplyr::relocate("comps", .before = "hsps") |>
    tidyr::replace_na(list(hsps = 0L)) |>
    tibble::new_tibble(class = "hsp")
}

#' @details
#' [write_hsp()] writes a HSP data.frame to a file.
#' @param x An outcome of [as_hsp()]
#' @param path A file name or connection to write to
#' @rdname hsp
#' @export
write_hsp = function(x, path = "hsp.txt") {
  stopifnot(inherits(x, "hsp"))
  .lines = c(
    "# HSP 1-false-negative ratio",
    "1.0",
    "# number of HSP data points",
    nrow(x),
    "# HSP data"
  )
  readr::write_lines(.lines, path)
  readr::write_tsv(x, path, na = "", append = TRUE, col_names = FALSE)
}

#' @rdname hsp
#' @export
read_hsp = function(path) {
  x = readr::read_tsv(path,
    col_names = c(hsp_keys, "comps", "hsps"),
    col_types = "iiiiii", skip = 5L, show_col_types = FALSE
  )[]
  class(x) = c("hsp", class(x))
  x
}

hsp_keys = c("cohort_i", "cohort_j", "location_i", "location_j")

count_hsp = function(captured) {
  sibs = filter_sibs(captured)
  fsp = group_by_fsp(sibs) |> count_coh_loc(name = "fsps")
  hsp = group_by_hsp(sibs) |> count_coh_loc(name = "hsps")
  dplyr::left_join(hsp, fsp, by = hsp_keys) |>
    dplyr::mutate(hsps = .data$hsps - 2L * dplyr::coalesce(.data$fsps, 0L), fsps = NULL)
}

group_by_hsp = function(sibs) {
  hs_father = sibs |>
    dplyr::select(!c("mother_id", "share_mother")) |>
    dplyr::rename(parent_id = "father_id") |>
    dplyr::filter(.data$share_father) |>
    dplyr::select(!"share_father")
  hs_mother = sibs |>
    dplyr::select(!c("father_id", "share_father")) |>
    dplyr::rename(parent_id = "mother_id") |>
    dplyr::filter(.data$share_mother) |>
    dplyr::select(!"share_mother")
  dplyr::bind_rows(hs_father, hs_mother) |>
    dplyr::arrange(.data$cohort, .data$location) |>
    dplyr::group_by(.data$parent_id)
}

group_by_fsp = function(sibs) {
  sibs |>
    dplyr::filter(.data$share_father & .data$share_mother) |>
    dplyr::select(!c("share_father", "share_mother")) |>
    dplyr::arrange(.data$cohort, .data$location) |>
    dplyr::group_by(.data$father_id, .data$mother_id) |>
    dplyr::filter(dplyr::n() > 1L)
}

count_coh_loc = function(x, name = NULL) {
  if (nrow(x) == 0L) {
    .names = c(hsp_keys, name %||% "n")
    return(tibble::new_tibble(rep(list(integer(0L)), 5L), names = .names))
  }
  x |>
    dplyr::group_modify(\(g, ...) {
      fun = \(v) {
        data.frame(
          cohort_i = g$cohort[v[1L]],
          cohort_j = g$cohort[v[2L]],
          location_i = g$location[v[1L]],
          location_j = g$location[v[2L]]
        )
      }
      utils::combn(seq_len(nrow(g)), 2L, fun, simplify = FALSE) |>
        purrr::list_rbind()
    }) |>
    dplyr::ungroup() |>
    dplyr::count(.data$cohort_i, .data$cohort_j, .data$location_i, .data$location_j, name = name)
}

count_hsp_comps = function(captured) {
  cnt = captured |>
    dplyr::count(.data$cohort, .data$location) |>
    tidyr::unite("cohloc", "cohort", "location") |>
    dplyr::mutate(cohloc = ordered(.data$cohloc, levels = unique(.data$cohloc)))
  tibble::tibble(
    df_i = cnt |> dplyr::rename_with(\(x) paste0(x, "_i")) |> list(),
    df_j = cnt |> dplyr::rename_with(\(x) paste0(x, "_j")) |> list()
  ) |>
    tidyr::unnest("df_i") |>
    tidyr::unnest("df_j") |>
    dplyr::filter(.data$cohloc_i <= .data$cohloc_j) |>
    dplyr::mutate(comps = ifelse(
      .data$cohloc_i == .data$cohloc_j, choose2(.data$n_i), .data$n_i * .data$n_j
    )) |>
    dplyr::select(!c("n_i", "n_j")) |>
    tidyr::separate("cohloc_i", c("cohort_i", "location_i"), convert = TRUE) |>
    tidyr::separate("cohloc_j", c("cohort_j", "location_j"), convert = TRUE)
}

filter_sibs = function(captured) {
  captured |>
    dplyr::mutate(
      share_father = duplicated(.data$father_id) | duplicated(.data$father_id, fromLast = TRUE),
      share_mother = duplicated(.data$mother_id) | duplicated(.data$mother_id, fromLast = TRUE)
    ) |>
    dplyr::filter(.data$share_father | .data$share_mother)
}

choose2 = function(n) {
  (n * (n - 1L)) %/% 2L
}

#' HSP format
#'
#' @details
#' `as_hsp` converts a result data frame to HSP format.
#' @param samples sample_family
#' @rdname hsp
#' @export
as_hsp = function(samples) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year)) |>
    dplyr::rename(cohort = "birth_year") |>
    dplyr::select(!"capture_year")
  comps = count_hsp_comps(captured)
  count_hsp(captured) |>
    dplyr::right_join(comps, by = c("cohort_i", "cohort_j", "location_i", "location_j")) |>
    dplyr::relocate("comps", .before = "hsps") |>
    tidyr::replace_na(list(hsps = 0L)) |>
    tibble::new_tibble(class = "hsp")
}

#' @details
#' `write_hsp` writes a HSP data frame to a file.
#' @param x An outcome of [as_hsp()]
#' @param path A file name or connection to write to
#' @rdname hsp
#' @export
write_hsp = function(x, path = "hsp.txt") {
  stopifnot(inherits(x, "hsp"))
  lines = c(
    "# HSP 1-false-negative ratio",
    "1.0",
    "# number of HSP data points",
    nrow(x),
    "# HSP data"
  )
  readr::write_lines(lines, path)
  readr::write_tsv(x, path, na = "", append = TRUE, col_names = FALSE)
}

#' @rdname hsp
#' @export
read_hsp = function(path) {
  x = readr::read_tsv(path,
    col_names = c("cohort_i", "cohort_j", "location_i", "location_j", "comps", "hsps"),
    col_types = "iiiiii", skip = 5L, show_col_types = FALSE
  )
  class(x) = c("hsp", class(x))
  x
}

count_hsp = function(captured) {
  hs_father = captured |>
    dplyr::filter(duplicated(.data$father_id) | duplicated(.data$father_id, fromLast = TRUE)) |>
    dplyr::rename(parent_id = "father_id") |>
    dplyr::select(!"mother_id")
  hs_mother = captured |>
    dplyr::filter(duplicated(.data$mother_id) | duplicated(.data$mother_id, fromLast = TRUE)) |>
    dplyr::rename(parent_id = "mother_id") |>
    dplyr::select(!"father_id")
  dplyr::bind_rows(hs_father, hs_mother) |>
    dplyr::arrange(.data$cohort, .data$location) |>
    dplyr::group_by(.data$parent_id) |>
    dplyr::group_modify(\(x, ...) {
      fun = \(v) {
        data.frame(
          cohort_i = x$cohort[v[1L]],
          cohort_j = x$cohort[v[2L]],
          location_i = x$location[v[1L]],
          location_j = x$location[v[2L]]
        )
      }
      utils::combn(seq_len(nrow(x)), 2L, fun, simplify = FALSE) |> purrr::list_rbind()
    }) |>
    dplyr::ungroup() |>
    dplyr::count(.data$cohort_i, .data$cohort_j, .data$location_i, .data$location_j, name = "hsps")
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
      .data$cohloc_i == .data$cohloc_j,
      .data$n_i * (.data$n_i - 1L) %/% 2L,
      .data$n_i * .data$n_j
    )) |>
    dplyr::select(!c("n_i", "n_j")) |>
    tidyr::separate("cohloc_i", c("cohort_i", "location_i"), convert = TRUE) |>
    tidyr::separate("cohloc_j", c("cohort_j", "location_j"), convert = TRUE)
}

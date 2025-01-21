#' HSP format
#'
#' @details
#' `as_hsp` converts a result data frame to HSP format.
#' @param samples sample_family
#' @rdname hsp
#' @export
as_hsp = function(samples) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year))
  comps = count_hsp_comps(captured)
  count_hsp(captured) |>
    dplyr::right_join(comps, by = hsp_keys) |>
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
    col_names = c(hsp_keys, "comps", "hsps"),
    col_types = "iiiiii", skip = 5L, show_col_types = FALSE
  )
  class(x) = c("hsp", class(x))
  x
}

hsp_keys = c("cohort_i", "cohort_j", "location_i", "location_j")

count_hsp = function(captured) {
  sibs = filter_sibs(captured)
  fsp = count_fsp(sibs)
  count_sp(sibs) |>
    dplyr::left_join(fsp, by = hsp_keys) |>
    dplyr::mutate(hsps = .data$hsps - 2L * dplyr::coalesce(.data$fsps, 0L), fsps = NULL)
}

count_sp = function(sibs) {
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
    dplyr::arrange(.data$birth_year, .data$location) |>
    dplyr::group_by(.data$parent_id) |>
    count_combination(name = "hsps")
}

count_fsp = function(sibs) {
  sibs |>
    dplyr::filter(.data$share_father & .data$share_mother) |>
    dplyr::select(!c("share_father", "share_mother")) |>
    dplyr::arrange(.data$birth_year, .data$location) |>
    dplyr::group_by(.data$father_id, .data$mother_id) |>
    dplyr::filter(dplyr::n() > 1L) |>
    count_combination(name = "fsps")
}

count_combination = function(x, name = NULL) {
  if (nrow(x) == 0L) {
    names = c(hsp_keys, name %||% "n")
    return(tibble::new_tibble(rep(list(integer(0L)), 5L), names = names))
  }
  x |>
    dplyr::group_modify(\(g, ...) {
      fun = \(v) {
        data.frame(
          cohort_i = g$birth_year[v[1L]],
          cohort_j = g$birth_year[v[2L]],
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
    dplyr::count(.data$birth_year, .data$location) |>
    tidyr::unite("cohloc", "birth_year", "location") |>
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
    dplyr::select(!"capture_year") |>
    dplyr::mutate(
      share_father = duplicated(.data$father_id) | duplicated(.data$father_id, fromLast = TRUE),
      share_mother = duplicated(.data$mother_id) | duplicated(.data$mother_id, fromLast = TRUE)
    ) |>
    dplyr::filter(.data$share_father | .data$share_mother)
}

choose2 = function(n) {
  (n * (n - 1L)) %/% 2L
}

#' HSP format
#'
#' @details
#' `as_hsp` converts a result data frame to HSP format.
#' @param .tbl sample_family
#' @rdname hsp
#' @export
as_hsp = function(.tbl) {
  .tbl = .tbl |>
    pairwise_half_sibling() |>
    summarize_hsp()
  class(.tbl) = c(class(.tbl), "hsp")
  .tbl
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
  class(x) = c(class(x), "hsp")
  x
}

pairwise_half_sibling = function(.tbl) {
  .tbl = dplyr::filter(.tbl, !is.na(.data$capture_year))
  tsv_i = .tbl |> dplyr::select(
    "id",
    mother_i = "mother_id",
    father_i = "father_id",
    cohort_i = "birth_year",
    location_i = "location"
  )
  tsv_j = .tbl |> dplyr::select(
    "id",
    mother_j = "mother_id",
    father_j = "father_id",
    cohort_j = "birth_year",
    location_j = "location"
  )
  tidyr::crossing(id_i = .tbl$id, id_j = .tbl$id) |>
    dplyr::filter(.data$id_i < .data$id_j) |>
    dplyr::left_join(tsv_i, by = c(id_i = "id")) |>
    dplyr::left_join(tsv_j, by = c(id_j = "id")) |>
    dplyr::mutate(is_hsp = (.data$mother_i == .data$mother_j) | (.data$father_i == .data$father_j))
}

summarize_hsp = function(.tbl) {
  .tbl |> dplyr::summarize(
    comps = dplyr::n(),
    hsps = sum(.data$is_hsp),
    .by = c("cohort_i", "cohort_j", "location_i", "location_j")
  )
}

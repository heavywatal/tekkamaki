#' Count actually sampled individuals
#'
#' @param samples A `sample_family` data.frame of [tekka()] result.
#' @returns A data.frame with the following columns:
#' - `capture_year`
#' - `location`
#' - `adult`
#' - `juvenile`
#' @rdname sampled
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y25 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' count_sampled(samples)
count_sampled = function(samples) {
  samples |>
    filter_sampled() |>
    dplyr::mutate(age = ifelse(.data$capture_age > 0L, "adult", "juvenile")) |>
    dplyr::count(.data$capture_year, .data$location, .data$age) |>
    tidyr::pivot_wider(names_from = "age", values_from = "n")
}

filter_sampled = function(samples) {
  samples |>
    dplyr::filter(!is.na(.data$capture_year)) |>
    dplyr::mutate(capture_age = .data$capture_year - .data$birth_year) |>
    dplyr::rename(cohort = "birth_year")
}

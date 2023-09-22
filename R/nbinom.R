#' Test negative_binomial_distribution in tekka simulation
#'
#' @details
#' `plot_nbinom` draws distributions with various parameter sets.
#' @rdname nbinom
#' @export
plot_nbinom = function(n = 10000L) {
  df = tidyr::crossing(
    n = n,
    size = c(1, 10, 100, 1000),
    mu = c(1, 10, 100, 1000)
  )
  df = dplyr::mutate(df, x = purrr::pmap(df, cpp_rnbinom)) |>
    dplyr::rename(k = !!"size") |>
    tidyr::unnest("x")
  df_summary = df |>
    dplyr::summarize(dplyr::across(dplyr::everything(), mean), .by = c("k", "mu"))
  ggplot2::ggplot(df) +
    ggplot2::aes(.data$x) +
    ggplot2::geom_vline(data = df_summary, ggplot2::aes(xintercept = .data$mu), colour = "tomato") +
    ggplot2::geom_vline(data = df_summary, ggplot2::aes(xintercept = .data$x), colour = "dodgerblue") +
    ggplot2::geom_histogram(bins = 50L) +
    ggplot2::facet_grid(ggplot2::vars(.data$k), ggplot2::vars(.data$mu), scale = "free_x", labeller = ggplot2::label_both)
}

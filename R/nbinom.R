#' Test negative_binomial_distribution in blackthunnus simulation
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
  df = dplyr::mutate(df, x = purrr::pmap(df, cpp_rnbinom)) %>%
    dplyr::rename(k = !!"size") %>%
    tidyr::unnest()
  df_summary = dplyr::group_by(df, .data$k, .data$mu) %>% dplyr::summarise_all(mean)
  ggplot2::ggplot(df, ggplot2::aes_string("x")) +
    ggplot2::geom_vline(data = df_summary, ggplot2::aes_string(xintercept = "mu"), colour = "tomato") +
    ggplot2::geom_vline(data = df_summary, ggplot2::aes_string(xintercept = "x"), colour = "dodgerblue") +
    ggplot2::geom_histogram(bins = 50L) +
    ggplot2::facet_grid(k ~ mu, scale = "free_x", labeller = ggplot2::label_both)
}

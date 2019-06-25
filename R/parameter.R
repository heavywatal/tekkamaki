#' Visualize parameter json file
#'
#' @param file path to a json file
#' @rdname parameter
#' @export
plot_parameters_json = function(file = NULL) {
  if (!is.null(file)) {
    message("TODO")
  }
  p_vec = read_vectors() %>%
    tidyr::gather(parameter, value, -age) %>%
    ggplot2::ggplot(ggplot2::aes_(~age, ~value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ parameter, scale = "free_y", ncol = 1L)
  p_mat = read_migration_matrices() %>%
    ggplot2::ggplot(ggplot2::aes_(~to, ~from, fill = ~value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::facet_wrap(~age)
  cowplot::plot_grid(p_vec, p_mat, nrow = 1L)
}

read_vectors = function() {
  tibble::tibble(
    natural_mortality = natural_mortality(),
    fishing_mortality = fishing_mortality(),
    weight_for_age = weight_for_age()[seq_along(natural_mortality)],
    age = seq_along(natural_mortality) / 4
  )
}

read_migration_matrices = function() {
  migration_matrices() %>%
    purrr::map_dfr(gather_migration_matrix, .id = "age") %>%
    dplyr::mutate(age = as.integer(.data$age) - 1L)
}

gather_migration_matrix = function(x) {
  reshape2::melt(x) %>%
    tibble::as_tibble() %>%
    dplyr::rename(from = .data$Var1, to = .data$Var2)
}

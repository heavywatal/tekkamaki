#' Visualize parameter json file
#'
#' @param file path to a json file
#' @rdname parameter
#' @export
plot_parameters_json = function(file = system.file("parameters.json", package = "tekkamaki")) {
  obj = read_parameters_json(file)
  p_vec = obj |>
    parameters_to_tbl() |>
    tidyr::pivot_longer(!"age", names_to = "parameter") |>
    ggplot2::ggplot() +
    ggplot2::aes(.data$age, .data$value) +
    ggplot2::geom_line() +
    ggplot2::labs(y = NULL) +
    ggplot2::facet_wrap(ggplot2::vars(.data$parameter), scale = "free_y", ncol = 1L)
  p_mat = obj |>
    parameters_tidy_matrices() |>
    ggplot2::ggplot() +
    ggplot2::aes(.data$to, .data$from, fill = .data$probability) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::facet_wrap(ggplot2::vars(.data$age))
  cowplot::plot_grid(p_vec, p_mat, nrow = 1L)
}

read_parameters_json = function(file) {
  obj = jsonlite::read_json(file, simplifyVector = TRUE)
  obj[["migration_matrices"]] = obj[["migration_matrices"]] |>
    apply(1, identity, simplify = FALSE)
  obj
}

parameters_to_tbl = function(obj) {
  nat_mor = obj[["natural_mortality"]]
  tibble::tibble(
    natural_mortality = nat_mor,
    fishing_mortality = obj[["fishing_mortality"]],
    weight_for_age = obj[["weight_for_age"]][seq_along(nat_mor)],
    age = seq_along(nat_mor) / 4
  )
}

parameters_tidy_matrices = function(obj) {
  obj[["migration_matrices"]] |>
    lapply(gather_migration_matrix) |>
    purrr::list_rbind(names_to = "age") |>
    dplyr::mutate(age = .data$age - 1L)
}

gather_migration_matrix = function(x) {
  dim_x = dim(x)
  dimnames(x) = list(from = seq_len(dim_x[1L]), to = seq_len(dim_x[2L]))
  cubelyr::as.tbl_cube(x, met_name = "probability") |>
    tibble::as_tibble()
}

#' @importFrom generics augment
#' @export
generics::augment

#' @param x sample_family data.frame.
#' @param layout A data.frame.
#' @param ... passed to layout function.
#' @rdname plot
#' @export
augment.sample_family = function(x, layout = NULL, ...) {
  augment(as_genealogy(x), layout = layout, ...)
}

#' @rdname plot
#' @export
augment.genealogy = function(x, layout = NULL, ...) {
  eattr = as.data.frame(x)
  if (is.null(layout)) {
    layout = layout_demography(eattr, ...)
  } else if (is.function(layout)) {
    layout = layout(eattr, ...)
  }
  if (is.data.frame(layout)) {
    stopifnot(utils::hasName(layout, c("id", "x", "y")))
  } else {
    stop("Invalid type '", typeof(layout), "' for argument 'layout'")
  }
  add_coordinates(eattr, layout) |>
    dplyr::mutate(
      xend = ifelse(is.na(.data$sampled), NA, .data$xend),
      yend = ifelse(is.na(.data$sampled), NA, .data$yend),
      label = .data$to
    )
}

add_coordinates = function(x, layout) {
  stopifnot(utils::hasName(layout, c("id", "x", "y")))
  lo_end = dplyr::rename_with(layout, \(x) paste0(x, "end"))
  x |>
    dplyr::left_join(layout, by = c(to = "id")) |>
    dplyr::left_join(lo_end, by = c(from = "idend"))
}

as_genealogy = function(x) {
  x = x |>
    tidyr::pivot_longer(dplyr::ends_with("_id")) |>
    dplyr::mutate(sampled = !is.na(.data$capture_year)) |>
    dplyr::select(from = "value", to = "id", "birth_year", "capture_year", "sampled") |>
    igraphlite::graph_from_data_frame()
  class(x) = c("genealogy", class(x))
  x
}

#' Methods for quick visualization
#'
#' @rdname plot
#' @export
layout_demography = function(x) {
  if (utils::hasName(x, "to")) x = dplyr::rename(x, id = "to")
  x |>
    dplyr::distinct(.data$id, .data$birth_year) |>
    dplyr::group_by(.data$birth_year) |>
    dplyr::mutate(x = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select("id", "x", y = "birth_year")
}

#' @param lwd passed to [ggplot2::geom_segment].
#' @param cex,pch passed to [ggplot2::geom_point] and [ggplot2::geom_text].
#' @rdname plot
#' @export
plot.genealogy = function(x, ..., lwd = 0.5, cex = 5, pch = 16) {
  data = augment(x, ...)
  f = function(x) dplyr::filter(x, !is.na(.data$xend))
  ggplot2::ggplot(data) +
    ggplot2::aes(.data$x, .data$y) +
    ggplot2::geom_segment(data = f, ggplot2::aes(xend = .data$xend, yend = .data$yend), linewidth = lwd, alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$sampled), shape = pch, size = cex) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = cex * 0.5)
}

#' @rdname plot
#' @export
plot.sample_family = plot.genealogy

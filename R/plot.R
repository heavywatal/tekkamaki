#' @importFrom generics augment
#' @export
generics::augment

#' Methods for quick visualization
#'
#' @description
#' Sample family trees and generated gene genealogies can be visualized briefly
#' with the [plot()] method. The output can be further customized with ggplot2
#' functions.
#'
#' The input can be also customized.
#' First, use [augment()] to add coordinates and other attributes for plotting.
#' Then you can modify them as needed.
#' Finally, build the plot with [ggplot2::ggplot()].
#' @param x A `sample_family` of `genealogy` data.frame.
#' @param layout A data.frame or function to compute coordinates.
#'   [layout_demography()] is applied by default.
#' @param ... Additional arguments passed to the layout function.
#' @returns `augment()` returns a data.frame suitable for plotting.
#' @rdname plot
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y25 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' augment(samples)
#'
#' plot(samples) +
#'   ggplot2::theme_void() +
#'   ggplot2::theme(legend.position = "top")
#'
#' genealogy = make_gene_genealogy(samples)
#' plot(genealogy) +
#'   ggplot2::theme_void() +
#'   ggplot2::theme(legend.position = "top")
augment.sample_family = function(x, layout = NULL, ...) {
  augment(as_genealogy(x), layout = layout, ...)
}

#' @rdname plot
#' @export
augment.genealogy = function(x, layout = NULL, ...) {
  eattr = annotate_sampled(x) |>
    as.data.frame() |>
    dplyr::arrange(nchar(.data$to), .data$to)
  if (is.null(layout)) {
    layout = layout_demography(eattr, ...)
  } else if (is.function(layout)) {
    layout = layout(eattr, ...)
  }
  if (is.data.frame(layout)) {
    stopifnot(c("id", "x", "y") %in% names(layout))
  } else {
    stop("Invalid type '", typeof(layout), "' for argument 'layout'", call. = FALSE)
  }
  add_coordinates(eattr, layout) |>
    dplyr::mutate(
      xend = ifelse(is.na(.data$sampled), NA, .data$xend),
      yend = ifelse(is.na(.data$sampled), NA, .data$yend),
      label = .data$to
    )
}

add_coordinates = function(x, layout) {
  stopifnot(c("id", "x", "y") %in% names(layout))
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

#' @returns [layout_demography()] computes coordinates for plotting genealogy.
#' @rdname plot
#' @export
layout_demography = function(x) {
  if ("to" %in% names(x)) x = dplyr::rename(x, id = "to")
  x |>
    dplyr::distinct(.data$id, .data$birth_year) |>
    dplyr::mutate(x = dplyr::row_number(), .by = "birth_year") |>
    dplyr::select("id", "x", y = "birth_year")
}

#' @param lwd passed to [ggplot2::geom_segment].
#' @param cex,pch passed to [ggplot2::geom_point] and [ggplot2::geom_text].
#' @returns [plot()] returns a ggplot2 object.
#' @rdname plot
#' @export
plot.genealogy = function(x, ..., lwd = 0.5, cex = 5, pch = 16) {
  .df = augment(x, ...)
  f = function(x) dplyr::filter(x, !is.na(.data$xend))
  ggplot2::ggplot(.df) +
    ggplot2::aes(.data$x, .data$y) +
    ggplot2::geom_segment(data = f, ggplot2::aes(xend = .data$xend, yend = .data$yend), linewidth = lwd, alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$sampled), shape = pch, size = cex) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = cex * 0.5)
}

#' @rdname plot
#' @export
plot.sample_family = plot.genealogy

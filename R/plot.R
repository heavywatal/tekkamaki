#' @importFrom generics augment
#' @export
generics::augment

#' @param x sample_family data.frame.
#' @param layout A data.frame.
#' @param ... passed to layout function.
#' @rdname plot
#' @export
augment.sample_family = function(x, layout = NULL, ...) {
  if (is.null(layout)) {
    layout = layout_demography(x, ...)
  } else if (is.function(layout)) {
    layout = layout(x, ...)
  }
  if (is.data.frame(layout)) {
    stopifnot(all(utils::hasName(layout, c("x", "y"))))
    stopifnot(nrow(layout) == nrow(x))
  } else {
    stop("Invalid type '", typeof(layout), "' for argument 'layout'")
  }
  el = as_edgelist(x)
  segment_df = utils::getFromNamespace("segment_df", "igraphlite")
  df = segment_df(el[["from"]], el[["to"]], layout[["x"]], layout[["y"]])
  num_samples = sum(!is.na(x[["capture_year"]]))
  df %>% dplyr::mutate(
    label = df$to,
    sampled = df$to <= num_samples
  )
}

#' Methods for quick visualization
#'
#' @rdname plot
#' @export
layout_demography = function(x) {
  df = dplyr::arrange(x, .data$id)
  stopifnot(df[["id"]] == seq_len(nrow(df)))
  df %>%
    dplyr::group_by(.data$birth_year) %>%
    dplyr::mutate(x = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(.data$x, y = .data$birth_year)
}

#' @param lwd passed to [ggplot2::geom_segment].
#' @param cex,pch passed to [ggplot2::geom_point] and [ggplot2::geom_text].
#' @rdname plot
#' @export
plot.sample_family = function(x, ..., lwd = 0.5, cex = 5, pch = 16) {
  data = augment(x, ...)
  ggplot2::ggplot(data, ggplot2::aes_(~x, ~y)) +
    ggplot2::geom_segment(ggplot2::aes_(xend = ~xend, yend = ~yend), size = lwd, alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes_(colour = ~sampled), shape = pch, size = cex) +
    ggplot2::geom_text(ggplot2::aes_(label = ~label), size = cex * 0.5)
}

#' Convert sample family data.frames to igraphlight objects
#'
#' Connections among samples and their ancestors are recorded in
#' `sample_family` data.frames.
#' [as_igraph()] converts them to igraphlite objects.
#' @param x A `sample_family` data.frame in the output of [tekka()].
#' @param ... Additional arguments passed to [igraphlite::graph_from_data_frame()].
#' @rdname graph
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' as_igraph(samples)
as_igraph.sample_family = function(x, ...) {
  as_edgelist(x) |>
    igraphlite::graph_from_data_frame(...)
}

#' @importFrom igraphlite as_igraph
#' @export
igraphlite::as_igraph

as_edgelist = function(x) {
  x |>
    dplyr::select(dplyr::ends_with("id")) |>
    tidyr::pivot_longer(dplyr::ends_with("_id"), names_to = NULL) |>
    dplyr::select(from = "value", to = "id")
}

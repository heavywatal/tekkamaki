#' Convert genealogy to igraph
#'
#' @details
#' `as_igraph` converts a result to igraph.
#' @param x `sample_family` table
#' @rdname graph
#' @export
as_igraph.sample_family = function(x) {
  as_edgelist(x) |>
    igraphlite::graph_from_data_frame()
}

#' @importFrom igraphlite as_igraph
#' @export
igraphlite::as_igraph

as_edgelist = function(x) {
  x |>
    dplyr::select(dplyr::ends_with("id")) |>
    dplyr::filter(.data$father_id != 0L, .data$mother_id != 0L) |>
    tidyr::gather("key", "from", dplyr::ends_with("_id")) |>
    dplyr::select("from", to = "id")
}

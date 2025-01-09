#' Functions to generate gene genealogy from sample family table
#'
#' [make_gene_genealogy()] generates a random gene genealogy from a table.
#' @param samples A data.frame: `sample_family` or output from [gather_segments()].
#' @rdname genealogy
#' @export
make_gene_genealogy = function(samples) {
  if (inherits(samples, "sample_family")) {
    samples = gather_segments(samples)
  }
  x = sample.int(2L, nrow(samples), replace = TRUE)
  df = samples |>
    dplyr::mutate(from = paste(.data$parent_id, x, sep = "-")) |>
    dplyr::select("from", to = "id", "birth_year", "capture_year")
  graph = igraphlite::graph_from_data_frame(df)
  class(graph) = c("genealogy", class(graph))
  graph
}

#' @details
#' [gather_segments()] transforms an individual-based `sample_family` into
#' a segment-based table.
#' @rdname genealogy
#' @export
gather_segments = function(samples) {
  id_cols = c("mother_id", "father_id")
  samples |>
    dplyr::select(!"location") |>
    dplyr::filter(.data$mother_id > 0L) |>
    tidyr::pivot_longer(tidyr::all_of(id_cols),
      names_to = "homolog",
      names_transform = \(x) as.integer(factor(x, levels = id_cols)),
      values_to = "parent_id"
    ) |>
    dplyr::arrange(-.data$birth_year, .data$id, .data$homolog) |>
    tidyr::unite("id", "id", "homolog", sep = "-")
}

#' @details
#' [count_uncoalesced()] counts uncoalesced roots of the samples.
#' @param genealogy An output from [make_gene_genealogy()].
#' @rdname genealogy
#' @export
count_uncoalesced = function(genealogy) {
  v_src = igraphlite::Vsource(genealogy)
  is_src = igraphlite::igraph_from(genealogy) %in% v_src
  annotate_sampled(genealogy)
  in_genealogy = !is.na(igraphlite::Eattr(genealogy)$sampled)
  sum(is_src & in_genealogy)
}

#' @details
#' [annotate_sampled()] adds "sampled" edge attribute: `TRUE` if sampled,
#' `FALSE` if upstream of samples, `NA` if unrelated.
#' @rdname genealogy
#' @export
annotate_sampled = function(genealogy) {
  if (!"sampled" %in% names(igraphlite::Eattr(genealogy))) {
    binary = edge_sampled(genealogy)
    v_to = igraphlite::igraph_to(genealogy)
    v_up = igraphlite::upstream_vertices(genealogy, v_to[binary])
    igraphlite::Eattr(genealogy)$sampled = ifelse(v_to %in% v_up, binary, NA)
  }
  genealogy
}

edge_sampled = function(genealogy) {
  !is.na(igraphlite::Eattr(genealogy)$capture_year)
}

areTRUE = function(x) !is.na(x) & x

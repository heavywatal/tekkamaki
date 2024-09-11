#' Functions to generate gene genealogy and SNP matrix
#'
#' @details
#' [make_snp()] is a shortcut to generate a SNP matrix from
#' a `sample_family` data.frame.
#' @param .tbl `sample_family` data.frame.
#' @param ss A sequence of `segsites`;
#'   its length is the number of segments;
#'   each element is the number of segsites on a segment.
#' @rdname snp
#' @export
make_snp = function(.tbl, ss = c(2L, 2L)) {
  segments = gather_segments(.tbl)
  matrices = lapply(ss, \(segsites) {
    make_gene_genealogy(segments) |> place_mutations(segsites)
  })
  Reduce(cbind, matrices)
}

#' @details
#' [gather_segments()] transforms an individual-based `sample_family` into
#' a segment-based table.
#' @rdname snp
#' @export
gather_segments = function(.tbl) {
  .tbl |>
    dplyr::select(!"location") |>
    dplyr::filter(.data$birth_year > 0L) |>
    tidyr::pivot_longer(dplyr::ends_with("_id"), names_to = "homolog", values_to = "parent_id") |>
    dplyr::mutate(homolog = c(mother_id = 1L, father_id = 2L)[.data$homolog]) |>
    dplyr::arrange(.data$id, .data$homolog) |>
    dplyr::mutate(id = paste(.data$id, .data$homolog, sep = "-"), homolog = NULL)
}

#' @details
#' [make_gene_genealogy()] generates a random gene genealogy from a
#' segment-based table.
#' @param segments An output from [gather_segments()].
#' @rdname snp
#' @export
make_gene_genealogy = function(segments) {
  x = sample.int(2L, nrow(segments), replace = TRUE)
  df = segments |>
    dplyr::mutate(from = paste(.data$parent_id, x, sep = "-")) |>
    dplyr::select("from", to = "id", "birth_year", "capture_year") |>
    dplyr::mutate(sampled = !is.na(.data$capture_year))
  graph = igraphlite::graph_from_data_frame(df)
  v_to = igraphlite::igraph_to(graph)
  v_sampled = v_to[df$sampled]
  v_genealogy = igraphlite::upstream_vertices(graph, v_sampled)
  igraphlite::Eattr(graph)$sampled = ifelse(v_to %in% v_genealogy, df$sampled, NA)
  class(graph) = c("genealogy", class(graph))
  graph
}

#' @details
#' [place_mutations()] generates a SNP matrix by randomly placing a fixed
#' number of mutations on a given genealogy.
#' @param segsites The number of segregating sites on a segment.
#' @rdname snp
#' @export
place_mutations = function(genealogy, segsites) {
  sampled = igraphlite::Eattr(genealogy)$sampled
  v_to = igraphlite::igraph_to(genealogy)
  v_sampled = v_to[areTRUE(sampled)]
  v_genealogy = v_to[!is.na(sampled)]
  origins = sample(v_genealogy, segsites, replace = TRUE)
  mutants = igraphlite::neighborhood(genealogy, origins, order = 1073741824L, mode = 1L)
  res = lapply(mutants, \(.x) v_sampled %in% .x) |>
    simplify2array(higher = FALSE)
  mode(res) = "integer"
  rownames(res) = igraphlite::Vnames(genealogy)[v_sampled]
  res
}

#' @details
#' [count_uncoalesced()] counts uncoalesced roots of the samples.
#' @param genealogy An output from [make_gene_genealogy()].
#' @rdname snp
#' @export
count_uncoalesced = function(genealogy) {
  v_src = igraphlite::Vsource(genealogy)
  is_src = igraphlite::igraph_from(genealogy) %in% v_src
  in_genealogy = !is.na(igraphlite::Eattr(genealogy)$sampled)
  sum(is_src & in_genealogy)
}

areTRUE = function(x) !is.na(x) & x

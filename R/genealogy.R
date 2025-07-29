#' Generate gene genealogy from samples
#'
#' An individual-based family tree stored in `sample_family` is transformed
#' into a gene-based table with [gather_segments()].
#' Then, the ancestry of each gene/segment is randomly assigned in [make_gene_genealogy()].
#' Sampled segments and their ancestors can be annotated with [annotate_sampled()].
#' @param samples A data.frame: `sample_family` or its tranfromation by [gather_segments()].
#' Using the latter will improve performance when the function is called many times,
#' e.g., in [make_snp()].
#' @returns An igraphlite object with `genealogy` subclass.
#' @rdname genealogy
#' @seealso [make_snp()] for SNP generation using genealogy.
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' segments = gather_segments(samples)
#' segments
#'
#' genealogy = make_gene_genealogy(segments)
#' genealogy
#'
#' count_uncoalesced(genealogy)
#'
#' annotate_sampled(genealogy)
make_gene_genealogy = function(samples) {
  if (inherits(samples, "sample_family")) {
    samples = gather_segments(samples)
  }
  x = sample.int(2L, nrow(samples), replace = TRUE)
  .df = samples |>
    dplyr::mutate(from = paste(.data$parent_id, x, sep = "-")) |>
    dplyr::select("from", to = "id", "birth_year", "capture_year")
  graph = igraphlite::graph_from_data_frame(.df)
  class(graph) = c("genealogy", class(graph))
  graph
}

#' @returns [gather_segments()] transforms an individual-based `sample_family`
#' into a segment-based table.
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

#' @returns [count_uncoalesced()] returns the count of uncoalesced roots of the genealogy.
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

#' @returns [annotate_sampled()] adds the "sampled" edge attribute to the genealogy:
#' `TRUE` if sampled, `FALSE` if upstream of samples, `NA` if unrelated.
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

#' Functions to generate gene genealogy and SNP matrix
#'
#' @details
#' [make_snp()] generates a SNP matrix.
#' @param .tbl `sample_family` data.frame.
#' @param ss A sequence of `segsites`;
#'   its length is the number of segments;
#'   each element is the number of segsites on a segment.
#' @rdname snp
#' @export
make_snp = function(.tbl, ss = c(2L, 2L)) {
  segments = gather_segments(.tbl)
  matrices = lapply(ss, function(segsites) {
    make_gene_genealogy(segments, segsites = segsites) %>% extract_snp()
  })
  unname(Reduce(cbind, matrices))
}

#' @details
#' [gather_segments()] transform individual-based `sample_family` into
#' segment-based table.
#' @rdname snp
#' @export
gather_segments = function(.tbl) {
  .tbl %>%
    dplyr::select(-"location") %>%
    tidyr::gather("homolog", "parent_id", -"id", -"birth_year", -"capture_year") %>%
    dplyr::mutate(homolog = c(mother_id = 1L, father_id = 2L)[.data$homolog]) %>%
    dplyr::arrange(.data$id, .data$homolog) %>%
    dplyr::mutate(id = paste(.data$id, .data$homolog, sep = "-"), homolog = NULL)
}

#' @details
#' [make_gene_genealogy()] generates random gene genealogy.
#' @param segments An output from [gather_segments()].
#' @param segsites The number of segregating sites on a segment.
#' @rdname snp
#' @export
make_gene_genealogy = function(segments, segsites = 0L) {
  n = nrow(segments)
  df = dplyr::transmute(
      segments,
      from = paste(.data$parent_id, sample.int(2L, n, replace = TRUE), sep = "-"),
      to = .data$id,
      .data$birth_year,
      .data$capture_year
    ) %>%
    mark_upstream(segsites = segsites)
  class(df) = c("genealogy", "tbl_df", "tbl", "data.frame")
  df
}

#' @details
#' [count_uncoalesced()] counts uncoalesced lineages in a gene genealogy
#' @param genealogy An output from [make_gene_genealogy()].
#' @rdname snp
#' @export
count_uncoalesced = function(genealogy) {
  origins = stringr::str_detect(genealogy[["from"]], "^0")
  on_tree = !is.na(genealogy[["sampled"]])
  sum(origins & on_tree)
}

mark_upstream = function(.tbl, segsites) {
  graph = .tbl %>%
    dplyr::filter(!stringr::str_detect(.data$from, "^0")) %>%
    igraphlite::graph_from_data_frame()
  v_sampled = graph$to[!is.na(graph$Eattr[["capture_year"]])]
  v_genealogy = igraphlite::upstream_vertices(graph, v_sampled)
  vn_genealogy = igraphlite::as_vnames(graph, v_genealogy)
  .tbl$sampled = ifelse(.tbl$to %in% vn_genealogy, !is.na(.tbl$capture_year), NA)
  if (segsites > 0L) {
    origins = sample(v_genealogy, segsites)
    mutants = igraphlite::neighborhood(graph, origins, order = 1073741824L, mode = 1L)
    .tbl$ss = purrr::map_dfc(mutants, function(.x) {
      as.integer(.tbl$to %in% igraphlite::as_vnames(graph, .x))
    })
  }
  .tbl
}

extract_snp = function(genealogy) {
  as.matrix(genealogy$ss[areTRUE(genealogy$sampled), ])
}

areTRUE = function(x) !is.na(x) & x

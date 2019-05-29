#' Functions to generate gene genealogy and SNP matrix
#'
#' @details
#' [make_snp()] generates a SNP matrix.
#' @param .tbl `sample_family` data.frame
#' @param segsites number of segregating sites
#' @rdname snp
#' @export
make_snp = function(.tbl, segsites = 1L) {
  chromosomes = gather_chromosome(.tbl)
  replicate(segsites, make_gene_genealogy(chromosomes) %>% extract_snp())
}

#' @details
#' [gather_chromosome()] transform individual-based `sample_family` into
#' chromosome-based table.
#' @rdname snp
#' @export
gather_chromosome = function(.tbl) {
  .tbl %>%
    dplyr::select(-"location") %>%
    tidyr::gather("chr", "parent_id", -"id", -"birth_year", -"capture_year") %>%
    dplyr::mutate(chr = c(mother_id = 1L, father_id = 2L)[.data$chr]) %>%
    dplyr::arrange(.data$id, .data$chr) %>%
    dplyr::mutate(id = paste(.data$id, .data$chr, sep = "-"), chr = NULL)
}

#' @details
#' [make_gene_genealogy()] generates random gene genealogy.
#' @param chromosomes The output from [gather_chromosome()]
#' @rdname snp
#' @export
make_gene_genealogy = function(chromosomes) {
  n = nrow(chromosomes)
  df = dplyr::transmute(
      chromosomes,
      from = paste(.data$parent_id, sample.int(2L, n, replace = TRUE), sep = "-"),
      to = .data$id,
      .data$birth_year,
      .data$capture_year
    ) %>%
    mark_upstream()
  class(df) = c("genealogy", "tbl_df", "tbl", "data.frame")
  df
}

mark_upstream = function(.tbl) {
  graph = .tbl %>%
    dplyr::filter(!stringr::str_detect(.data$from, "^0")) %>%
    igraphlite::graph_from_data_frame()
  v_sampled = graph$to[!is.na(graph$Eattr[["capture_year"]])]
  v_genealogy = igraphlite::upstream_vertices(graph, v_sampled)
  origin = sample(v_genealogy, 1L)
  mutants = igraphlite::neighborhood(graph, origin, order = 1073741824L, mode = 1L)[[1L]]
  mutants = igraphlite::as_vnames(graph, mutants)
  v_genealogy = igraphlite::as_vnames(graph, v_genealogy)
  .tbl$sampled = ifelse(.tbl$to %in% v_genealogy, !is.na(.tbl$capture_year), NA)
  .tbl$mutated = (.tbl$to %in% mutants)
  .tbl
}

extract_snp = function(genealogy) {
  as.integer(genealogy$mutated[areTRUE(genealogy$sampled)])
}

areTRUE = function(x) !is.na(x) & x

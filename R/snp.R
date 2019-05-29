#' Functions to process SNPs
#'
#' @details
#' `make_snp` makes snp table from a result.
#' @param .tbl sample_family
#' @param lambda mutation rate per haploid per generation
#' @param rho recombination rate per generation
#' @rdname snp
#' @export
make_snp = function(.tbl, lambda, rho = 0) {
  if (!missing(rho)) {
    stop("Recombination has not been implemented yet")
  }
  .tbl %>%
    gather_chromosome() %>%
    sprinkle_mutations(lambda = lambda) %>%
    accumulate_mutations() %>%
    complete_genotype()
}

# row chromosome
gather_chromosome = function(.tbl) {
  .tbl %>%
    dplyr::select(-"location") %>%
    tidyr::gather("chr", "parent_id", -"id", -"birth_year", -"capture_year") %>%
    dplyr::mutate(chr = c(mother_id = 1L, father_id = 2L)[.data$chr]) %>%
    dplyr::arrange(.data$id, .data$chr)
}

make_gene_genealogy = function(.tbl) {
  n = nrow(.tbl)
  dplyr::transmute(
    .tbl,
    parent = paste(parent_id, sample.int(2L, n, replace = TRUE), sep = "-"),
    id = paste(id, chr, sep = "-"),
    .data$birth_year,
    is_sampled = !is.na(.data$capture_year)
  ) %>%
  filter_connected()
}

filter_connected = function(.tbl) {
  g = .tbl %>%
    dplyr::filter(!stringr::str_detect(.data$parent, "^0")) %>%
    igraphlite::graph_from_data_frame()
  v_sampled = g$to[g$Eattr[["is_sampled"]]]
  v_genealogy = igraphlite::upstream_vertices(g, v_sampled)
  v_genealogy = igraphlite::as_vnames(g, v_genealogy)
  dplyr::filter(.tbl, .data$id %in% v_genealogy)
}

# choose random mutation positions [0, 1)
sprinkle_mutations = function(.tbl, lambda) {
  stopifnot(all(c("chr", "parent_id") %in% names(.tbl)))
  dplyr::mutate(
    .tbl,
    num_mutation = stats::rpois(dplyr::n(), lambda),
    pos = purrr::map(.data$num_mutation, stats::runif, min = 0.0, max = 1.0),
    num_mutation = NULL
  )
}

.accumulate_mutations_f = function(.id, .chr, .tbl) {
  .row = dplyr::filter(.tbl, .data$id == .id, .data$chr == .chr)
  .pos = .row$pos
  .id = .row$parent_id
  while (.id != 0L) {
    .chr = sample.int(2L, 1L)
    .row = dplyr::filter(.tbl, .data$id == .id, .data$chr == .chr)
    .pos = c(.pos, .row$pos)
    .id = .row$parent_id
  }
  purrr::flatten_dbl(.pos)
}

# trace back ancestors to collect mutation positions
accumulate_mutations = function(.tbl) {
  .tbl %>%
    dplyr::filter(!is.na(.data$capture_year)) %>%
    dplyr::transmute(
      .data$id,
      .data$chr,
      pos = purrr::map2(.data$id, .data$chr, .accumulate_mutations_f, .tbl = .tbl)
    )
}

# complete genotypes for all the positions
complete_genotype = function(.tbl) {
  .tbl %>%
    dplyr::mutate(genotype = 1L) %>%
    tidyr::unnest() %>%
    tidyr::complete_(c("id", "chr", "pos"), fill = list(genotype = 0L))
}

# spread data frame to wide format
spread_genotype = function(.tbl) {
  .tbl %>%
    dplyr::mutate(pos = sprintf("snp%04d", .data$pos %>% as.factor() %>% as.integer())) %>%
    tidyr::spread("pos", "genotype", fill = 0L)
}

#' Functions to generate SNPs on given samples.
#'
#' @details
#' [make_snp()] generates a list of SNP data.frames by calling
#' [make_snp_chromosome()] in parallel.
#' Use `RNGkind("L'Ecuyer-CMRG")` and `set.seed()` to get reproducible results.
#' The number of CPU cores used can be configured via `mc.cores` option.
#' @inheritParams make_gene_genealogy
#' @param ss A sequence of `segsites`;
#'   its length is the number of chromosome;
#'   each element is the number of segsites on a chromosome.
#'   If a named vector is given, the output is also named.
#' @rdname snp
#' @export
make_snp = function(samples, ss) {
  on.exit(stats::runif(1L)) # proxy of parallel::nextRNGStream(.Random.seed)
  if (inherits(samples, "sample_family")) {
    samples = gather_segments(samples)
  }
  parallel::mclapply(ss, \(segsites) {
    make_gene_genealogy(samples) |> make_snp_chromosome(segsites)
  })
}

#' @details
#' [make_snp_chromosome()] simulate a SNP data.frame on a chromosome by calling
#' [place_mutations()] and `recombination()`.
#' @rdname snp
#' @export
make_snp_chromosome = function(genealogy, segsites) {
  plan = genealogy |> prepare_snp(segsites)
  v_sam = igraphlite::igraph_to(genealogy)[edge_sampled(genealogy)]
  snp_tbls = plan |> purrr::pmap(\(segsites, vt) {
    x = place_mutations(genealogy, segsites, v_sam)
    if (length(vt) > 0L) {
      recombination(genealogy, vt)
    }
    x
  })
  Reduce(cbind, snp_tbls)
}

#' @details
#' [place_mutations()] generates a SNP data.frame by randomly placing a fixed
#' number of mutations on a given genealogy.
#' It means that all the sites are perfectly linked with each other.
#' @param segsites The number of segregating sites on a segment.
#' @param v_sampled The sampled vertices. Use this to fix the output order.
#' @rdname snp
#' @export
place_mutations = function(genealogy, segsites, v_sampled = NULL) {
  if (segsites < 1L) {
    return(NULL)
  }
  v_to = igraphlite::igraph_to(genealogy)
  if (is.null(v_sampled)) {
    v_sampled = v_to[edge_sampled(genealogy)]
  }
  annotate_sampled(genealogy)
  v_up = v_to[!is.na(igraphlite::Eattr(genealogy)$sampled)]
  origins = sample(v_up, segsites, replace = TRUE)
  mutants = igraphlite::neighborhood(genealogy, origins, order = 1073741824L, mode = 1L)
  lapply(mutants, \(.x) as.integer(v_sampled %in% .x)) |>
    stats::setNames(seq_len(segsites)) |>
    as.data.frame(row.names = igraphlite::Vnames(genealogy)[v_sampled])
}

prepare_snp = function(genealogy, segsites) {
  to = igraphlite::igraph_to(genealogy)
  flat = tibble::tibble(
    segsites = rmultinom1(segsites, length(to) + 1L),
    vt = c(sample(to, replace = FALSE), NA_integer_)
  )
  nested = flat |>
    dplyr::mutate(group = cumsum(.data$segsites)) |>
    dplyr::summarize(segsites = .data$segsites[1L], vt = list(.data$vt), .by = "group") |>
    dplyr::select(!"group") |>
    dplyr::filter(.data$segsites > 0L)
  nested$vt[nrow(nested)] = list(integer(0))
  nested
}

prepare_recombination = function(genealogy) {
  eattr = igraphlite::Eattr(genealogy)
  if (!"vh" %in% names(eattr)) {
    vnames = igraphlite::Vnames(genealogy)
    vf = igraphlite::igraph_from(genealogy)
    homolog = switch_homolog(vnames[vf])
    eattr$vh = match(homolog, vnames)
    igraphlite::Eattr(genealogy) = eattr
  }
  if ("sampled" %in% names(eattr)) {
    eattr$sampled = NULL
    igraphlite::Eattr(genealogy) = eattr
  }
  eattr
}

# [recombination()] rewires `from` end of the given edge.
# Note that genealogy is modified in-place.
# Edge IDs are not preserved (igraph#2677) while vertices remain the same.
recombination = function(genealogy, vt) {
  to = igraphlite::igraph_to(genealogy)
  edges = match(vt, to)
  vf = igraphlite::igraph_from(genealogy)[edges]
  eattr = prepare_recombination(genealogy)
  rows = eattr[edges, ]
  new_edges = matrix(c(rows$vh, vt), nrow = 2L, byrow = TRUE)
  igraphlite::delete_edges(genealogy, edges)
  igraphlite::add_edges(genealogy, as.vector(new_edges))
  rows$vh = vf
  ecount = length(to)
  start = ecount - length(vt) + 1L
  igraphlite::Eattr(genealogy)[seq.int(start, ecount), ] = rows
  genealogy
}

switch_homolog = function(name) {
  stringr::str_sub(name, -1L) = ifelse(stringr::str_ends(name, "1"), "2", "1")
  name
}

rmultinom1 = function(n, k) {
  stats::rmultinom(1L, n, rep_len(1, k)) |> as.vector()
}

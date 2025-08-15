#' Generate SNPs on given samples.
#'
#' @description
#' [place_mutations()] generates a SNP data.frame by randomly placing a fixed
#' number of mutations on a given genealogy.
#' It means that all the sites are perfectly linked with each other.
#'
#' [make_snp_chromosome()] simulates a SNP data.frame on a chromosome using
#' [place_mutations()] with recombination.
#' [make_snp()] calls it for each chromosome in parallel.
#'
#' Use `RNGkind("L'Ecuyer-CMRG")` and `set.seed()` to get reproducible results.
#' The number of CPU cores used can be configured via `mc.cores` option.
#' @inheritParams make_gene_genealogy
#' @param ss A sequence of `segsites`;
#'   its length is the number of chromosome;
#'   each element is the number of segsites on a chromosome.
#'   If a named vector is given, the output is also named.
#' @returns [make_snp()] returns a list of data.frame for each chromosome.
#' @seealso [write_vcf()] to write the output in VCF format.
#' @rdname snp
#' @export
#' @examples
#' RNGkind("L'Ecuyer-CMRG")
#' set.seed(666)
#' result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' segments = gather_segments(samples)
#' genealogy = make_gene_genealogy(segments)
#'
#' place_mutations(genealogy, 3L) |> str()
#'
#' ss = c(chr1 = 3, chr2 = 2)
#' make_snp(segments, ss) |> str()
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
#' It is assumed that each node in the genealogy has one recombination event
#' on each chromosome. In other words, a chromosome has a fixed number of
#' recombination events, equal to the number of nodes in the genealogy.
#' Their locations are randomly assigned, and segregating sites are placed among them.
#' For example, a chromosome with 3 segsites on a genealogy with 10 nodes can be
#' illustrated like this:
#' `5' rrrSrrSrrrrSr 3'`.
#' @returns [make_snp_chromosome()] and [place_mutations()] returns
#' a data.frame with segregating sites in columns, and sample segments in rows.
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
  purrr::list_cbind(snp_tbls, name_repair = "minimal")
}

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
  vn = igraphlite::Vnames(genealogy)[v_sampled]
  lapply(mutants, \(.x) as.integer(v_sampled %in% .x)) |>
    as.data.frame(row.names = vn, check.names = FALSE, fix.empty.names = FALSE)
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
  .start = ecount - length(vt) + 1L
  igraphlite::Eattr(genealogy)[seq.int(.start, ecount), ] = rows
  genealogy
}

switch_homolog = function(name) {
  stringr::str_sub(name, -1L) = ifelse(stringr::str_ends(name, "1"), "2", "1")
  name
}

rmultinom1 = function(n, k) {
  stats::rmultinom(1L, n, rep_len(1, k)) |> as.vector()
}

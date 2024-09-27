#' Functions to generate SNP matrix
#'
#' @details
#' [make_snp()] is a shortcut to generate a SNP matrix from
#' a `sample_family` data.frame.
#' @inheritParams make_gene_genealogy
#' @param ss A sequence of `segsites`;
#'   its length is the number of segments;
#'   each element is the number of segsites on a segment.
#' @rdname snp
#' @export
make_snp = function(samples, ss = c(2L, 2L)) {
  segments = gather_segments(samples)
  matrices = lapply(ss, \(segsites) {
    make_gene_genealogy(segments) |> make_snp_chromosome(segsites)
  })
  Reduce(cbind, matrices)
}

#' @details
#' [make_snp_chromosome()] simulate a SNP matrix on a chromosome by calling
#' [place_mutations()] and `recombination()`.
#' @rdname snp
#' @export
make_snp_chromosome = function(genealogy, segsites) {
  snp_tbl = genealogy |> prepare_snp(segsites)
  v_sam = igraphlite::igraph_to(genealogy)[edge_sampled(genealogy)]
  matrices = snp_tbl |> purrr::pmap(\(segsites, vt) {
    x = place_mutations(genealogy, segsites, v_sam)
    if (length(vt) > 0L) {
      recombination(genealogy, vt)
    }
    x
  })
  Reduce(cbind, matrices)
}

#' @details
#' [place_mutations()] generates a SNP matrix by randomly placing a fixed
#' number of mutations on a given genealogy.
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
  v_up = igraphlite::upstream_vertices(genealogy, v_sampled)
  origins = sample(v_up, segsites, replace = TRUE)
  mutants = igraphlite::neighborhood(genealogy, origins, order = 1073741824L, mode = 1L)
  res = lapply(mutants, \(.x) v_sampled %in% .x) |>
    simplify2array(higher = FALSE)
  mode(res) = "integer"
  rownames(res) = igraphlite::Vnames(genealogy)[v_sampled]
  res
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

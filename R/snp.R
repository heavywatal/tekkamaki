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
#' [gather_segments()] transforms an individual-based `sample_family` into
#' a segment-based table.
#' @rdname snp
#' @export
gather_segments = function(.tbl) {
  .tbl |>
    dplyr::select(!"location") |>
    tidyr::pivot_longer(dplyr::ends_with("_id"), names_to = "homolog", values_to = "parent_id") |>
    dplyr::mutate(homolog = c(mother_id = 1L, father_id = 2L)[.data$homolog]) |>
    dplyr::arrange(-.data$birth_year, .data$id, .data$homolog) |>
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
    dplyr::mutate(from = stringr::str_replace(.data$from, "^0-[12]$", "0")) |>
    dplyr::select("from", to = "id", "birth_year", "capture_year")
  graph = igraphlite::graph_from_data_frame(df)
  v0 = igraphlite::as_vids(graph, "0")
  igraphlite::delete_vertices(graph, v0)
  class(graph) = c("genealogy", class(graph))
  graph
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

#' @details
#' [count_uncoalesced()] counts uncoalesced roots of the samples.
#' @param genealogy An output from [make_gene_genealogy()].
#' @rdname snp
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
#' @rdname snp
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

areTRUE = function(x) !is.na(x) & x

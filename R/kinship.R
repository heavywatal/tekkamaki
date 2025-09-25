#' Analyze kinship within samples
#'
#' @details
#' [find_kinship()] finds kinship below given order.
#' @param samples sample_family
#' @param order integer
#' @param experimental boolean
#' @returns A data.frame with the following columns:
#' - `from`, `to`: sample IDs in integer.
#' - `path`: text representation of the path found between samples;
#'   ascending with `1` and descending with `0`;
#'   the count of the same paths is indicated by `_n` suffix;
#'   e.g., "10_1" and "10_2" means half-sibling and full-sibling pairs, respectively.
#' - `degree`: the number of steps in the path.
#' - `label`: aliases for the path.
#'   See <https://github.com/heavywatal/tekkamaki/blob/main/data-raw/kinlabels.R>
#'   for the available labels.
#' @seealso [as_hsp()] and [as_hsp2()] to find half-sibling pairs.
#' @seealso [as_pop()] and [as_pop2()] to find parent-offspring pairs.
#' @rdname kinship
#' @export
#' @examples
#' set.seed(666)
#' result = tekka("-y25 -l2 --sa 2,2 --sj 2,2")
#' samples = result$sample_family[[1L]]
#' find_kinship(samples, order = 3L)
find_kinship = function(samples, order = 4L, experimental = FALSE) {
  captured = dplyr::filter(samples, !is.na(.data$capture_year))
  max_age = 31L
  oldest_birth_year = suppressWarnings(min(captured$birth_year))
  threshold_birth_year = oldest_birth_year - max_age * order
  graph = samples |>
    dplyr::filter(.data$birth_year > threshold_birth_year) |>
    as_igraph()
  nodes = captured$id
  vids = igraphlite::as_vids(graph, nodes)
  .pairs = neighbor_pairs(graph, vids, order = order)
  if (nrow(.pairs) == 0L) {
    message("No kinship found")
    return(invisible(.pairs))
  }
  if (experimental) {
    find_kinship_common(graph, .pairs, order)
  } else {
    find_kinship_shortest(samples, graph, .pairs, order)
  }
}

neighbor_pairs = function(graph, vids, order) {
  # NOTE: mode = 3L can include down-up pairs
  kins = igraphlite::neighborhood(graph, vids, order = order, mode = 3L, mindist = 1L) |>
    lapply(filter_in_vids, vids = vids)
  tibble::tibble(from = vids, to = kins) |>
    tidyr::unnest("to") |>
    dplyr::filter(.data$from < .data$to) |>
    dplyr::arrange(.data$from, .data$to)
}

filter_in_vids = function(x, vids) {
  x = as.integer(x)
  x[x %in% vids]
}

find_kinship_shortest = function(samples, graph, pairs, order) {
  paths = find_shortest_paths(graph, pairs)
  birth_year = samples$birth_year[order(samples$id)]
  paths |>
    dplyr::mutate(path = lapply(.data$path, \(x) as.integer(diff(birth_year[x]) < 0L))) |>
    dplyr::filter(purrr::map_lgl(.data$path, \(x) length(rle(x)$lengths) < 3L & !identical(x, c(0L, 1L)))) |>
    label_kinship()
}

# add columns: path and degree
find_shortest_paths = function(graph, pairs) {
  nested_pairs = pairs |>
    dplyr::summarize(to = list(!!as.name("to")), .by = "from")
  .path = nested_pairs |>
    purrr::pmap(\(from, to) {
      igraphlite::get_all_shortest_paths(graph, from = from, to = to, mode = 3)
    })
  vnames = igraphlite::Vnames(graph)
  tibble::tibble(from = nested_pairs$from, path = .path) |>
    tidyr::unnest("path") |>
    dplyr::mutate(
      path = lapply(.data$path, \(x) vnames[x]),
      to = purrr::map_int(.data$path, \(x) x[length(x)]),
      from = vnames[.data$from]
    ) |>
    dplyr::filter(!purrr::map_lgl(.data$path, \(x) 0L %in% x))
}

find_kinship_common = function(graph, pairs, order) {
  vnames = igraphlite::Vnames(graph)
  pairs |>
    dplyr::mutate(
      data = purrr::pmap(pairs, \(from, to) count_updown(graph, from, to, order)),
      from = vnames[.data$from],
      to = vnames[.data$to]
    ) |>
    tidyr::unnest("data") |>
    dplyr::mutate(degree = .data$up + .data$down) |>
    dplyr::filter(.data$up + .data$down <= order) |>
    purrr::modify(as.integer) |>
    dplyr::mutate(
      ancestor = vnames[.data$ancestor],
      path = encode_updown(.data$up, .data$down)
    ) |>
    dplyr::select("from", "to", "ancestor", "path", "degree")
}

count_updown = function(graph, from, to, order) {
  vlist = igraphlite::neighborhood(graph, c(from, to), order = order, mode = 2L)
  common_ancestors = intersect(vlist[[1L]], vlist[[2L]])
  distances = if (length(common_ancestors) > 0L) {
    igraphlite::distances(graph, from = common_ancestors, to = c(from, to))
  } else {
    matrix(numeric(0L), nrow = 0L, ncol = 2L)
  }
  tibble::tibble(
    ancestor = common_ancestors,
    up = distances[, 1L],
    down = distances[, 2L]
  )
}

encode_updown = function(up, down) {
  purrr::map2_chr(up, down, .encode_updown_impl)
}

.encode_updown_impl = function(up, down) {
  .rle = list(lengths = c(up, down), values = c("1", "0"))
  paste(inverse.rle(.rle), collapse = "")
}

label_kinship = function(kinship) {
  kinship |>
    dplyr::mutate(path = purrr::map_chr(.data$path, stringr::str_flatten)) |>
    dplyr::filter(stringr::str_detect(.data$path, "01", negate = TRUE)) |>
    dplyr::count(.data$from, .data$to, .data$path) |>
    dplyr::mutate(
      degree = nchar(.data$path),
      path = paste(.data$path, .data$n, sep = "_"),
      n = NULL,
      label = factor(kinlabels[.data$path], levels = unique(kinlabels))
    )
}

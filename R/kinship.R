#' Analyze kinship with igraph
#'
#' @details
#' `find_kinship` finds kinship below given order.
#' @param .tbl sample_family
#' @param order integer
#' @param experimental boolean
#' @rdname kinship
#' @export
find_kinship = function(.tbl, order = 4L, experimental = FALSE) {
  graph = as_igraph(.tbl)
  nodes = dplyr::filter(.tbl, !is.na(.data$capture_year))$id
  vids = igraphlite::as_vids(graph, nodes)
  pairs = neighbor_pairs(graph, vids, order = order)
  if (nrow(pairs) == 0L) {
    message("No kinship found")
    return(invisible(pairs))
  }
  if (experimental) {
    find_kinship_common(graph, pairs, order)
  } else {
    find_kinship_shortest(.tbl, graph, pairs, order)
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

find_kinship_shortest = function(.tbl, graph, pairs, order) {
  paths = find_shortest_paths(graph, pairs)
  birth_year = .tbl$birth_year[order(.tbl$id)]
  paths |>
    dplyr::mutate(path = lapply(.data$path, \(x) as.integer(diff(birth_year[x]) < 0L))) |>
    dplyr::filter(purrr::map_lgl(.data$path, \(x) length(rle(x)$lengths) < 3L && !identical(x, c(0L, 1L)))) |>
    label_kinship()
}

# add columns: path and degree
find_shortest_paths = function(graph, pairs) {
  nested_pairs = pairs |>
    dplyr::summarize(to = list(!!as.name("to")), .by = "from")
  .path = nested_pairs |>
    purrr::pmap(\(from, to) {
      igraphlite::get_all_shortest_paths(graph, from, to, mode = 3)
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
    igraphlite::distances(graph, common_ancestors, c(from, to))
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

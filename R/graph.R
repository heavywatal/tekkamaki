#' Functions for igraph class
#'
#' @details
#' `as_igraph` converts a result to igraph.
#' @param .tbl sample_family
#' @rdname graph
#' @export
as_igraph = function(.tbl) {
  .tbl %>%
    dplyr::select(dplyr::ends_with("id")) %>%
    tidyr::gather("key", "parent_id", dplyr::ends_with("_id")) %>%
    dplyr::select("parent_id", "id") %>%
    igraphlite::graph_from_data_frame()
}

#' @details
#' `find_kinship` finds kinship below given order.
#' @param order integer
#' @param experimental boolean
#' @rdname graph
#' @export
find_kinship = function(.tbl, order = 4L, experimental = FALSE) {
  graph = as_igraph(.tbl)
  pairs = neighbor_pairs(graph, graph$sink, order = order)
  if (nrow(pairs) == 0L) {
    message("No kinship found")
    return(invisible(pairs))
  }
  if (experimental) {
    find_kinship_common(graph, pairs, order)
  } else {
    find_kinship_shortest(graph, pairs, order)
  }
}

neighbor_pairs = function(graph, vids, order) {
  kins = igraphlite::neighborhood(graph, vids, order = order, mode = 3L, mindist = 1L) %>%
    lapply(filter_in_vids, vids = vids)
  tibble::tibble(from = vids, to = kins) %>%
    tidyr::unnest() %>%
    dplyr::filter(.data$from < .data$to) %>%
    dplyr::arrange(.data$from, .data$to)
}

filter_in_vids = function(x, vids) {
  x = as.integer(x)
  x[x %in% vids]
}

find_kinship_shortest = function(graph, pairs, order) {
  paths = find_shortest_paths(graph, pairs)
  birth_year = .tbl$birth_year[order(.tbl$id)]
  paths %>%
    dplyr::mutate(path = purrr::map(.data$path, ~ as.integer(diff(birth_year[.x]) < 0L))) %>%
    dplyr::filter(purrr::map_lgl(.data$path, ~ length(rle(.x)$lengths) < 3L && !identical(.x, c(0L, 1L)))) %>%
    label_kinship()
}

# add columns: path and degree
find_shortest_paths = function(graph, pairs) {
  nested_pairs = pairs %>%
    dplyr::group_by(.data$from) %>%
    dplyr::summarise(to = list(!!as.name("to")))
  .path = purrr::pmap(nested_pairs, igraphlite::get_all_shortest_paths, graph = graph, mode = 3)
  tibble::tibble(from = nested_pairs$from, path = .path) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      path = lapply(.data$path, igraphlite::as_vnames, graph = graph),
      to = purrr::map_int(.data$path, ~ .x[length(.x)]),
      from = igraphlite::as_vnames(graph, .data$from)
    ) %>%
    dplyr::filter(!purrr::map_lgl(.data$path, ~ 0L %in% .x))
}

find_kinship_common = function(graph, pairs, order) {
  pairs %>%
    dplyr::mutate(
      data = purrr::pmap(., get_short_paths, graph = graph, order = order),
      from = igraphlite::as_vnames(graph, .data$from),
      to = igraphlite::as_vnames(graph, .data$to)
    ) %>%
    tidyr::unnest()
}

get_short_paths = function(graph, from, to, order) {
  count_updown(graph, from, to, order = order) %>%
    dplyr::mutate(degree = .data$up + .data$down) %>%
    dplyr::filter(.data$degree <= order) %>%
    dplyr::transmute(path = encode_updown(.data$up, .data$down), .data$degree)
}

count_updown = function(graph, from, to, order) {
  vlist = igraphlite::neighborhood(graph, c(from, to), order = order, mode = 2L)
  common_ancestors = Reduce(intersect, vlist)
  igraphlite::shortest_paths(graph, common_ancestors, c(from, to)) %>%
    as.data.frame() %>%
    stats::setNames(c("up", "down")) %>%
    tibble::rownames_to_column("ancestor") %>%
    dplyr::mutate_all(as.integer) %>%
    tibble::as_tibble()
}

encode_updown = function(up, down) {
  purrr::map2_chr(up, down, .encode_updown_impl)
}

.encode_updown_impl = function(up, down) {
  .rle = list(lengths = c(up, down), values = c("1", "0"))
  paste(inverse.rle(.rle), collapse = "")
}

label_kinship = function(kinship) {
  kinship %>%
    dplyr::mutate(path = purrr::map_chr(.data$path, paste0, collapse = "")) %>%
    dplyr::filter(stringr::str_detect(.data$path, "01", negate = TRUE)) %>%
    dplyr::count(.data$from, .data$to, .data$path) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      degree = nchar(.data$path),
      path = paste(.data$path, .data$n, sep = "_"),
      n = NULL,
      label = factor(kinlabels[.data$path], levels = unique(kinlabels))
    )
}

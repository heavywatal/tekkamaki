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
#' @rdname graph
#' @export
find_kinship = function(.tbl, order = 4L) {
  graph = as_igraph(.tbl)
  nodes = dplyr::filter(.tbl, !is.na(.data$capture_year))$id
  vids = igraphlite::as_vids(graph, nodes)
  birth_year = stats::setNames(.tbl$birth_year, .tbl$id)
  find_kinship_impl(graph, vids, order = order) %>%
    find_shortest_paths(graph) %>%
    dplyr::mutate(path = purrr::map(.data$path, ~ as.integer(diff(birth_year[.x]) < 0L))) %>%
    dplyr::filter(purrr::map_lgl(.data$path, ~ length(rle(.x)$lengths) < 3L && !identical(.x, c(0L, 1L)))) %>%
    label_kinship()
}

# find pairs
find_kinship_impl = function(graph, vids, order) {
  kins = igraphlite::neighborhood(graph, vids, order = order, mode = 3L) %>%
    lapply(function(x) x[x %in% vids])
  tibble::tibble(from = vids, to = kins) %>%
    tidyr::unnest() %>%
    dplyr::filter(.data$from < .data$to) %>%
    dplyr::arrange(.data$from, .data$to)
}

# add columns: path and degree
find_shortest_paths = function(kinship, graph) {
  .to = split(kinship$to, kinship$from)
  .from = as.integer(names(.to))
  .path = purrr::map2(.from, .to, igraphlite::get_all_shortest_paths, graph = graph, mode = 3)
  tibble::tibble(from = .from, path = .path) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      path = lapply(.data$path, igraphlite::as_vnames, graph = graph),
      to = purrr::map_chr(.data$path, ~ .x[length(.x)]),
      from = igraphlite::as_vnames(graph, .data$from)
    ) %>%
    dplyr::filter(!purrr::map_lgl(.data$path, ~ "0" %in% .x))
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

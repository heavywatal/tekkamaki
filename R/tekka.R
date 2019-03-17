#' Run C++ simulation.
#' @param args command line arguments as a string vector.
#' @rdname tekka
#' @export
tekka = function(args = character(0L)) {
  args = strsplit(args, "\\s+") %>% unlist() %>% as.character()
  .out = cpp_tekka(args)
  if (length(.out) == 0L) return(invisible(NULL))
  tibble::tibble(
    sample_family = list(read_result(.out[["sample_family"]])),
    demography = list(readr::read_tsv(.out[["demography"]]))
  )
}

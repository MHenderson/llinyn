#' Chop a String of Text to a Specific Width
#'
#' @param s
#' @param textwidth
#'
#' @return
#' @export
gqg_paragraph <- function(s, textwidth) {
  # we want to put a space every line length characters
  provisional_indices <- seq(textwidth, stringr::str_length(s), textwidth)

  # but if some of those positions are occupied by non-space
  # characters then we have to change plans slightly
  indices <- purrr::map_dbl(provisional_indices, locate_last_space_before, s = s)

  # do the actual changes here
  for(i in indices) {
    s <- replace_at_i(s, i)
  }

  return(s)
}

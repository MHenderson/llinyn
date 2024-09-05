#' Escape A Few Things
#'
#' Replace certain characters in a string with escaped versions of those same
#' characters for the benefit of LaTeX.
#'
#' @param x A text string.
#'
#' @return A text string with some escaped characters.
#' @export
repair_latex_string <- function(x) {
  x <- stringr::str_replace_all(x, "\\&", "\\\\&")
  x <- stringr::str_replace_all(x, "\\_", "\\\\_")
  x <- stringr::str_replace_all(x, "\\#", "")
  x <- stringr::str_replace_all(x, "\\$", "\\\\textdollar")
  return(x)
}

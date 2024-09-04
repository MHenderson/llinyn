#' Escape A Few Things
#'
#' Replace certain characters in a string with escaped versions of those same
#' characters for the benefit of LaTeX.
#'
#' @param x A text strirng.
#'
#' @return A text string with some escaped characters.
#' @export
repair_latex_string <- function(x) {
  x <- stringr::str_replace_all(text, "\\&", "\\\\&")
  x <- stringr::str_replace_all(tex, "\\_", "\\\\_")
  x <- stringr::str_replace_all(tex, "\\#", "")
  x <- stringr::str_replace_all(tex, "\\$", "\\\\textdollar")
  return(x)
}

#' Repair LaTeX
#'
#' @param X A data frame with a text column.
#'
#' @return A new data frame containing a tex column.
#' @export
repair_latex <- function(X) {
  X |>
    dplyr::mutate(
      tex = repair_latex_string(text)
    )
}

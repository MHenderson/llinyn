#' Replace a Character at a Location in a String
#'
#' @param s A string.
#' @param i An index.
#' @param x A character.
#'
#' @return The string s but with character x inserted at position i.
#' @export
replace_at_i <- function(s, i, x = "\n") {
  paste(
    stringr::str_sub(s, 1, i - 1),
    stringr::str_sub(s, i + 1, stringr::str_length(s)),
    sep = x
  )
}

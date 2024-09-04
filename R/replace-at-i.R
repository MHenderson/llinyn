#' Replace a Character at a Location in a String
#'
#' @param s
#' @param i
#' @param x
#'
#' @return
#' @export
replace_at_i <- function(s, i, x = "\n") {
  paste(
    stringr::str_sub(s, 1, i - 1),
    stringr::str_sub(s, i + 1, stringr::str_length(s)),
    sep = x
  )
}

#' Remove Datetime Strings from Text
#'
#' Removes all datetime headings like "% Tue 3 Mar 09:20:25 GMT 2020".
#'
#' @param text A string of text, possibly containing datetime headings.
#'
#' @return The input string with all datetime headings removed.
#' @export
strip_time_headings <- function(text) {
  text <- stringr::str_replace(text, "% [A-Za-z]+\\s+\\d+ [A-Za-z]+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", "")
  text <- stringr::str_replace(text, "% [A-Za-z]+\\s+[A-Za-z]+\\s+\\d+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", "")
  return(text)
}

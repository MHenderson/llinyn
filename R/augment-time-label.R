#' Remove Datetime Strings from Text
#'
#' Removes all datetime headings like "% Tue 3 Mar 09:20:25 GMT 2020".
#'
#' @param text A string of text, possibly containing datetime headings.
#'
#' @return The input string with all datetime headings removed.
strip_time_headings <- function(text) {
  text <- stringr::str_replace(text, "% [A-Za-z]+\\s+\\d+ [A-Za-z]+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", "")
  text <- stringr::str_replace(text, "% [A-Za-z]+\\s+[A-Za-z]+\\s+\\d+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", "")
  return(text)
}

#' Add Time Labels to a Data Frame of Text
#'
#' @param X A data frame with a column named text.
#'
#' @return A data frame with new columns, ts_s, ts, time_label.
#' @export
augment_time_label <- function(X) {
  X |>
    dplyr::mutate(
      ts_s = stringr::str_match(text, "% (.*?) (GMT|BST)")[, 2],
      ts = lubridate::parse_date_time(ts_s, orders = c("a d b H:M:S", "a b d H:M:S"))
    ) |>
    dplyr::mutate(
      time_label = format(ts, format = "%I:%M %p")
    ) |>
    dplyr::mutate(
      text = strip_time_headings(text)
    )
}

#' Match, Parse and Format Timestamps from Text
#'
#' This function reads and parses timestamps from text and then outputs a
#' differently formatted string representing the same timestamp.
#'
#' @param text Input text.
#'
#' @return A formatted timestamp string.
#' @export
extract_time_label <- function(text) {
  ts_s <- stringr::str_match(text, "% (.*?) (GMT|BST)")[, 2]
  ts <- lubridate::parse_date_time(ts_s, orders = c("a d b H:M:S", "a b d H:M:S"))
  format(ts, format = "%I:%M %p")
}

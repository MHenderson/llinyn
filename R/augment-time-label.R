#' Augment time labels
#'
#' @param X A dataframe
#'
#' @return
#' @export
#'
#' @examples
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
      text = stringr::str_replace(text, "% [A-Za-z]+\\s+\\d+ [A-Za-z]+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", ""),
      text = stringr::str_replace(text, "% [A-Za-z]+\\s+[A-Za-z]+\\s+\\d+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", ""),
    )
}

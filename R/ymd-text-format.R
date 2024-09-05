#' Convert a Date Into Text.
#'
#' This takes a ymd (e.g. 2024/09/01) and returns the same date in text format
#' (e.g. Sunday 1 September 2024).
#'
#' @param ymd A date in ymd format.
#'
#' @return A date string.
ymd_text_format <- function(ymd) {
  paste(
    lubridate::wday(ymd, label = TRUE, abbr = FALSE),
    lubridate::day(ymd),
    lubridate::month(ymd, label = TRUE, abbr = FALSE),
    lubridate::year(ymd)
  )
}

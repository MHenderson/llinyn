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

#' Create a LaTeX Section Heading
#'
#' Given a date in ymd format (e.g. 2024/09/01) and time string (e.g. 06:06 am)
#' this function returns a LaTeX section heading.
#'
#' @param ymd A date in ymd format.
#' @param time_s A time string.
#'
#' @return A LaTeX section heading string.
#' @export
latex_section_heading <- function(ymd, time_s) {
  paste0("\\section{", ymd_text_format(ymd), "}\n\n", "\\hspace*{\\fill}", time_s, "\\vspace{5mm}\n\n")
}

#' Add headings
#'
#' Given a data frame with columns `ymd`, `time_label` and `tex` return a data
#' frame with
#' the same columns but with an addition `tex_w_heading` column made from the `ymd`
#' and `time_label` columns.
#'
#' @param X A data frame with columns `ymd`, `time_label` and `tex`.
#'
#' @return A data frame with an extra column `tex_w_heading`.
#' @export
add_heading <- function(X) {
  X |>
    dplyr::mutate(
      tex_w_heading = paste0(latex_section_heading(ymd, time_label), tex)
    )
}

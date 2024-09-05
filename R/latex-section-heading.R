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

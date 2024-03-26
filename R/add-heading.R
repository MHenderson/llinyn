#' Add headings
#'
#' @param X A dateframe
#'
#' @return
#' @export
#'
#' @examples
add_heading <- function(X) {
  X |>
    dplyr::mutate(
      heading = paste(
        lubridate::wday(ymd, label = TRUE, abbr = FALSE),
        lubridate::day(ymd),
        lubridate::month(ymd, label = TRUE, abbr = FALSE),
        lubridate::year(ymd)
      )
    ) |>
    dplyr::mutate(
      tex_w_heading = paste0("\\section{", heading, "}\n\n", "\\hspace*{\\fill}", time_label, "\\vspace{5mm}\n\n", tex)
    )
}

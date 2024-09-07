#' Repair LaTeX
#'
#' @param X A data frame with a text column.
#'
#' @return A new data frame containing a tex column.
#' @export
repair_latex <- function(X) {
  X |>
    dplyr::mutate(
      tex = repair_latex_string(text)
    )
}

#' Left align text
#'
#' @param X A dataframe
#' @param textwidth An integer
#'
#' @return
#' @export
#'
#' @examples
left_align <- function(X, textwidth = 72) {
  X |>
    dplyr::mutate(
      tex = purrr::map_chr(tex, gqg, textwidth = textwidth)
    )
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
      time_label = extract_time_label(text),
            text = strip_time_headings(text)
    )
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
      tex_w_heading = paste0("\\section{", ymd_text_format(ymd), "}\n\n", "\\hspace*{\\fill}", time_label, "\\vspace{5mm}\n\n", tex)
    )
}

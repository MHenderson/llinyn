gqg <- function(x, textwidth) {

  paragraphs <- stringr::str_split(x, pattern = "(\n){2,}")[[1]]

  # remove empty paragraphs
  paragraphs <- paragraphs[!(paragraphs == "")]

  too_wide <- function(x) {
    stringr::str_length(x) > textwidth
  }

  # apply gqg_paragraph to those paragraphs wider than textwidth
  fixed_paras <- purrr::map_if(paragraphs, too_wide, gqg_paragraph, textwidth)

  fixed_paras <- as.character(fixed_paras)

  return(paste0(fixed_paras, collapse = "\n\n"))

}

gqg_paragraph <- function(s, textwidth) {
  # we want to put a space every line length characters
  provisional_indices <- seq(textwidth, stringr::str_length(s), textwidth)

  # but if some of those positions are occupied by non-space
  # characters then we have to change plans slightly
  indices <- purrr::map_dbl(provisional_indices, locate_last_space_before, s = s)

  # do the actual changes here
  for(i in indices) {
    s <- replace_at_i(s, i)
  }

  return(s)
}

locate_last_space_before <- function(s, x) {
  X <- stringr::str_locate_all(s, " ")[[1]]
  X_end <- X[, "end"]
  max(X_end[X_end <= x])
}

replace_at_i <- function(s, i, x = "\n") {
  paste(
    stringr::str_sub(s, 1, i - 1),
    stringr::str_sub(s, i + 1, stringr::str_length(s)),
    sep = x
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

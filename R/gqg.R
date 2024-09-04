#' Chop a String of Text to a Specific Width
#'
#' @param x
#' @param textwidth
#'
#' @return
#' @export
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

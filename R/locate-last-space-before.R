#' Locate Last Space Before a Space in a String
#'
#' @param s A string.
#' @param x An index.
#'
#' @return The position of the last space character in s before x.
#' @export
locate_last_space_before <- function(s, x) {
  X <- stringr::str_locate_all(s, " ")[[1]]
  X_end <- X[, "end"]
  max(X_end[X_end <= x])
}

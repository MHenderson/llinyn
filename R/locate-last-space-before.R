locate_last_space_before <- function(s, x) {
  X <- stringr::str_locate_all(s, " ")[[1]]
  X_end <- X[, "end"]
  max(X_end[X_end <= x])
}

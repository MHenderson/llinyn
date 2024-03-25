left_align <- function(X, textwidth = 72) {
  X |>
    dplyr::mutate(
      tex = purrr::map_chr(tex, gqg, textwidth = textwidth)
    )
}

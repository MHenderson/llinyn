repair_latex <- function(X) {
  X |>
    dplyr::mutate(
      tex = stringr::str_replace_all(text, "\\&", "\\\\&"),
      tex = stringr::str_replace_all(tex, "\\_", "\\\\_"),
      tex = stringr::str_replace_all(tex, "\\#", ""),
      # we really only should replace lone dollars, not those that denote equations
      tex = stringr::str_replace_all(tex, "\\$", "\\\\textdollar")
    )
}

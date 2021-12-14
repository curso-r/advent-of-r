# The resulting polymer isn't nearly strong enough to reinforce the submarine.
# You'll need to run more steps of the pair insertion process; a total of 40
# steps should do it.
#
# In the above example, the most common element is B (occurring 2192039569602
# times) and the least common element is H (occurring 3849876073 times);
# subtracting these produces 2188189693529.
#
# Apply 40 steps of pair insertion to the polymer template and find the most and
# least common elements in the result. What do you get if you take the quantity
# of the most common element and subtract the quantity of the least common
# element?

# Keep track of first and last elems of the original
orig <- "data-raw/14b_extended_polymerization.txt" |>
  readr::read_lines(n_max = 1) |>
  stringr::str_replace("^(.).*?(.)$", "\\1\\2") |>
  stringr::str_split("") |>
  purrr::pluck(1)

# Read template as a table already counting pairs
poly <- "data-raw/14b_extended_polymerization.txt" |>
  readr::read_lines(n_max = 1) |>
  stringr::str_split("") |>
  purrr::pluck(1) |>
  purrr::accumulate(~paste0(stringr::str_sub(.x, -1), .y)) |>
  utils::tail(-1) |>
  tibble::tibble() |>
  purrr::set_names("pair") |>
  dplyr::count(pair)

# Read rules as a table
rules <- "data-raw/14b_extended_polymerization.txt" |>
  readr::read_table(skip = 1, col_names = FALSE) |>
  purrr::set_names("pair", "rm", "insertion") |>
  dplyr::select(-rm) |>
  dplyr::mutate(insertion = stringr::str_replace(
    pair, "(.)(.)", paste0("\\1", insertion, "\\2")
  ))

# Run one round of insertions
do_insertions <- function(poly) {
  poly |>
    dplyr::left_join(rules, "pair") |>
    dplyr::mutate(
      insertion = purrr::map(insertion, stringr::str_extract, c("^..", "..$"))
    ) |>
    tidyr::unnest(insertion) |>
    dplyr::group_by(pair = insertion) |>
    dplyr::summarise(n = sum(n))
}

# Rum do_insertions() 40 times and do most common el - least common el
40 |>
  seq_len() |>
  purrr::reduce(~do_insertions(.x), .init = poly) |>
  dplyr::mutate(elem = stringr::str_split(pair, "")) |>
  tidyr::unnest(elem) |>
  dplyr::group_by(elem) |>
  dplyr::summarise(n = sum(n)) |>
  dplyr::mutate(
    n = ifelse(elem %in% orig, n + 1, n),
    n = n / 2
  ) |>
  dplyr::filter(n == max(n) | n == min(n)) |>
  dplyr::pull(n) |>
  purrr::reduce(`-`) |>
  abs() |>
  format(scientific = FALSE)

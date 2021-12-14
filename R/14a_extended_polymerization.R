# The incredible pressures at this depth are starting to put a strain on your
# submarine. The submarine has polymerization equipment that would produce
# suitable materials to reinforce the submarine, and the nearby
# volcanically-active caves should even have the necessary input elements in
# sufficient quantities.
#
# The submarine manual contains instructions for finding the optimal polymer
# formula; specifically, it offers a polymer template and a list of pair
# insertion rules (your puzzle input). You just need to work out what polymer
# would result after repeating the pair insertion process a few times.
#
# For example:
#
# NNCB
#
# CH -> B
# HH -> N
# CB -> H
# NH -> C
# HB -> C
# HC -> B
# HN -> C
# NN -> C
# BH -> H
# NC -> B
# NB -> B
# BN -> B
# BB -> N
# BC -> B
# CC -> N
# CN -> C
#
# The first line is the polymer template - this is the starting point of the
# process.
#
# The following section defines the pair insertion rules. A rule like AB -> C
# means that when elements A and B are immediately adjacent, element C should be
# inserted between them. These insertions all happen simultaneously.
#
# So, starting with the polymer template NNCB, the first step simultaneously
# considers all three pairs:
#
#   - The first pair (NN) matches the rule NN -> C, so element C is inserted
#     between the first N and the second N.
#   - The second pair (NC) matches the rule NC -> B, so element B is inserted
#     between the N and the C.
#   - The third pair (CB) matches the rule CB -> H, so element H is inserted
#     between the C and the B.
#
# Note that these pairs overlap: the second element of one pair is the first
# element of the next pair. Also, because all pairs are considered
# simultaneously, inserted elements are not considered to be part of a pair
# until the next step.
#
# After the first step of this process, the polymer becomes NCNBCHB.
#
# Here are the results of a few steps using the above rules:
#
# Template:     NNCB
# After step 1: NCNBCHB
# After step 2: NBCCNBBBCBHCB
# After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
# After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#
# This polymer grows quickly. After step 5, it has length 97; After step 10, it
# has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H
# occurs 191 times, and N occurs 865 times; taking the quantity of the most
# common element (B, 1749) and subtracting the quantity of the least common
# element (H, 161) produces 1749 - 161 = 1588.
#
# Apply 10 steps of pair insertion to the polymer template and find the most and
# least common elements in the result. What do you get if you take the quantity
# of the most common element and subtract the quantity of the least common
# element?

# Read template as a string
poly <- readr::read_lines("data-raw/14a_extended_polymerization.txt", n_max = 1)

# Read rules as a table
rules <- "data-raw/14a_extended_polymerization.txt" |>
  readr::read_table(skip = 1, col_names = FALSE) |>
  purrr::set_names("pair", "rm", "insertion") |>
  dplyr::select(-rm) |>
  dplyr::mutate(insertion = stringr::str_replace(
    pair, "(.)(.)", paste0("\\1", insertion, "\\2")
  ))

# Run one round of insertions
do_insertions <- function(poly) {
  poly |>
    stringr::str_split("") |>
    purrr::pluck(1) |>
    purrr::accumulate(~paste0(stringr::str_sub(.x, -1), .y)) |>
    utils::tail(-1) |>
    purrr::map_chr(~rules[rules$pair == .x, ]$insertion) |>
    purrr::reduce(~paste0(.x, stringr::str_sub(.y, -2))) |>
    stringr::str_c(collapse = "")
}

# Rum do_insertions() 10 times and do most common el - least common el
10 |>
  seq_len() |>
  purrr::reduce(~do_insertions(.x), .init = poly) |>
  stringr::str_split("") |>
  table() |>
  {\(t) list(t[which.max(t)], t[which.min(t)])}() |>
  purrr::reduce(`-`) |>
  abs() |>
  unname()

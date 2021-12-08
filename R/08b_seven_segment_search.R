# Through a little deduction, you should now be able to determine the remaining
# digits. Consider again the first example above:
#
# acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
# cdfeb fcadb cdfeb cdbaf
#
# After some careful analysis, the mapping between signal wires and segments
# only make sense in the following configuration:
#
#  dddd
# e    a
# e    a
#  ffff
# g    b
# g    b
#  cccc
#
# So, the unique signal patterns would correspond to the following digits:
#
#   - acedgfb: 8
#   - cdfbe: 5
#   - gcdfa: 2
#   - fbcad: 3
#   - dab: 7
#   - cefabd: 9
#   - cdfgeb: 6
#   - eafb: 4
#   - cagedb: 0
#   - ab: 1
#
# Then, the four digits of the output value can be decoded:
#
#   - cdfeb: 5
#   - fcadb: 3
#   - cdfeb: 5
#   - cdbaf: 3
#
# Therefore, the output value for this entry is 5353.
#
# Following this same process for each entry in the second, larger example
# above, the output value of each entry can be determined:
#
#   - fdgacbe cefdb cefbgd gcbe: 8394
#   - fcgedb cgb dgebacf gc: 9781
#   - cg cg fdcagb cbg: 1197
#   - efabcd cedba gadfec cb: 9361
#   - gecf egdcabf bgf bfgea: 4873
#   - gebdcfa ecba ca fadegcb: 8418
#   - cefg dcbef fcge gbcadfe: 4548
#   - ed bcgafe cdgba cbgef: 1625
#   - gbdfcae bgc cg cgb: 8717
#   - fgae cfgab fg bagce: 4315
#
# Adding all of the output values in this larger example produces 61229.
#
# For each entry, determine all of the wire/segment connections and decode the
# four-digit output values. What do you get if you add up all of the output
# values?

# Decode one line of output values
decode <- function(entry) {

  # Find and split a parttern that has a certain str_len()
  find_by_len <- function(patterns, len) {
    patterns |>
      magrittr::extract(stringr::str_length(patterns) == len) |>
      stringr::str_split("") |>
      purrr::pluck(1)
  }

  # Reference frequencies
  ref_freq <- list(
    "a" = 8,
    "b" = 6,
    "c" = 8,
    "d" = 7,
    "e" = 4,
    "f" = 9,
    "g" = 7
  )

  # Reference values
  ref_val <- list(
    "abdefg" = 6,
    "abcefg" = 0,
    "cf" = 1,
    "acdfg" = 3,
    "abcdfg" = 9,
    "abcdefg" = 8,
    "bcdf" = 4,
    "acf" = 7,
    "abdfg" = 5,
    "acdeg" = 2
  )

  # Calculate current frequencies
  cur_freq <- entry |>
    dplyr::select(P01:P10) |>
    purrr::flatten_chr() |>
    stringr::str_split("") |>
    purrr::flatten_chr() |>
    table()

  # Create dictionary to translate segments
  dict <- list()

  # Translate segments with unique frequencies
  dict[["e"]] <- names(cur_freq[cur_freq == 4])
  dict[["b"]] <- names(cur_freq[cur_freq == 6])
  dict[["f"]] <- names(cur_freq[cur_freq == 9])

  # Extract patterns from entry
  patterns <- entry |>
    dplyr::select(P01:P10) |>
    purrr::flatten_chr()

  # Determine remaining segment from 1
  one <- find_by_len(patterns, 2)
  dict[["c"]] <- one[!one %in% purrr::flatten_chr(dict)]

  # Determine remaining segment from 7
  seven <- find_by_len(patterns, 3)
  dict[["a"]] <- seven[!seven %in% purrr::flatten_chr(dict)]

  # Determine remaining segment from 4
  four <- find_by_len(patterns, 4)
  dict[["d"]] <- four[!four %in% purrr::flatten_chr(dict)]

  # Determine last remaining segment
  dict[["g"]] <- names(cur_freq)[!names(cur_freq) %in% purrr::flatten_chr(dict)]

  # Translate segments output values
  entry |>
    dplyr::select(V01:V04) |>
    purrr::flatten_chr() |>
    stringr::str_split("") |>
    purrr::map(~names(dict)[match(.x, dict)]) |>
    purrr::map(sort) |>
    purrr::map(stringr::str_c, collapse = "") |>
    purrr::map(~purrr::flatten_chr(ref_val)[match(.x, names(ref_val))]) |>
    purrr::flatten_chr() |>
    as.integer() |>
    stringr::str_c(collapse = "") |>
    as.numeric()
}

# Read input, map decode() and sum all output values
"data-raw/08b_seven_segment_search.txt" |>
  readr::read_delim(" ", col_names = NULL) |>
  purrr::set_names(
    paste0("P", stringr::str_pad(1:10, 2, "left", "0")), "remove",
    paste0("V", stringr::str_pad(1:4, 2, "left", "0"))
  ) |>
  dplyr::select(-remove) |>
  tibble::rowid_to_column("id") |>
  tidyr::nest(entry = c(P01:V04)) |>
  dplyr::mutate(output = purrr::map_dbl(entry, decode)) |>
  dplyr::summarise(output = sum(output)) |>
  dplyr::pull(output)

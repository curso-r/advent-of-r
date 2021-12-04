# On the other hand, it might be wise to try a different strategy: let the giant
# squid win.
#
# You aren't sure how many bingo boards a giant squid could play at once, so
# rather than waste time counting its arms, the safe thing to do is to figure
# out which board will win last and choose that one. That way, no matter which
# boards it picks, it will win for sure.
#
# In the above example, the second board is the last to win, which happens after
# 13 is eventually called and its middle column is completely marked. If you
# were to keep playing until this point, the second board would have a sum of
# unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
#
# Figure out which board will win last. Once it wins, what would its final score
# be?

# Parse numbers drawn
draws <- "data-raw/04b_giant_squid.txt" |>
  readr::read_lines(n_max = 1) |>
  stringr::str_split(",") |>
  purrr::pluck(1) |>
  as.numeric()

# Convert columns of a matrix into rows and bind
cols_to_rows <- function(df) {
  df |>
    dplyr::select(-board, -id) |>
    as.matrix() |>
    t() |>
    tibble::as_tibble(rownames = "id") |>
    purrr::set_names("id", paste0("C", 1:5)) |>
    dplyr::mutate(board = df$board) |>
    dplyr::bind_rows(df) |>
    dplyr::relocate(board, id) |>
    purrr::set_names("id", "board", paste0("N", 1:5))
}

# Calculate score of the looser board
loosing_score <- function(df, draws) {

  # Mark number currently drawn with NAs (rows and cols)
  df <- dplyr::mutate(df, dplyr::across(c(N1:N5), dplyr::na_if, draws[1]))

  # Filter possible complete rows or cols
  win <- dplyr::filter(df, dplyr::if_all(c(N1:N5), is.na))

  # If there is a complete row or col, filter out the board
  if (nrow(win) > 0) {

    # If there was only one board left, calculate its score
    if (length(unique(df$id)) == 1) {
      print(df)

      # Get looser board, sum unmarked numbers and multiply by draws[1]
      output <- df |>
        dplyr::filter(stringr::str_starts(board, "R")) |>
        dplyr::select(-id, -board) |>
        purrr::flatten_dbl() |>
        sum(na.rm = TRUE) |>
        magrittr::multiply_by(draws[1])

      # Return loosing score
      return(output)
    }

    # Filter out winning boards
    df <- dplyr::filter(df, !id %in% win$id)
  }

  # Recurse on to next drawn
  loosing_score(df, draws[-1])
}

# Read boards, create one row for every row and col, and mark with NAs
"data-raw/04a_giant_squid.txt" |>
  readr::read_table(skip = 1, col_names = paste0("C", 1:5)) |>
  dplyr::mutate(board = (dplyr::row_number() - 1) %/% 5) |>
  dplyr::group_by(board) |>
  dplyr::mutate(id = paste0("R", 1:5)) |>
  dplyr::group_split() |>
  purrr::map_dfr(cols_to_rows) |>
  loosing_score(draws)

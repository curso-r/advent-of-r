# Finish folding the transparent paper according to the instructions. The manual
# says the code is always eight capital letters.
#
# What code do you use to activate the infrared thermal imaging camera system?

# Read table of where the dots are
dots <- "2021/data-raw/13b_transparent_origami.txt" |>
  readr::read_lines() |>
  stringr::str_subset("^[0-9]") |>
  tibble::tibble() |>
  purrr::set_names("dot") |>
  tidyr::separate(dot, c("x", "y"), ",") |>
  dplyr::mutate_all(as.integer) |>
  dplyr::mutate_all(`+`, 1L)

# Read folding instructions
instructions <- "2021/data-raw/13b_transparent_origami.txt" |>
  readr::read_lines() |>
  stringr::str_subset("^[^0-9]") |>
  tibble::tibble() |>
  purrr::set_names("fold") |>
  tidyr::separate(fold, c("axis", "line"), "=") |>
  dplyr::mutate(
    axis = stringr::str_sub(axis, -1),
    line = as.integer(line) + 1L
  )

# Place the dots on the paper
paper <- matrix(FALSE, nrow = max(dots$y), ncol = max(dots$x))
for (i in seq_len(nrow(dots))) {
  paper[dots$y[i], dots$x[i]] <- TRUE
}

# Iterate over all instructions
for (i in seq_len(nrow(instructions))) {

  # Get fold's axis and line
  axis <- instructions$axis[i]
  line <- instructions$line[i]

  # Fold acording to axis
  if (axis == "x") {

    # Number of cols to the right of the fold
    size <- length((line + 1):dim(paper)[2])

    # Get cols to the right of the fold, flip them and OR them with left side
    paper[, (line - size):(line - 1)] <-
      paper[, (line + 1):(line + size)][, size:1] |
      paper[, (line - size):(line - 1)]

    # Discard cols representing folded paper
    paper <- paper[, 1:(line - 1)]

  } else {

    # Number of rows under the fold
    size <- length((line + 1):dim(paper)[1])

    # Get rows under the fold, flip them and AND them with top side
    paper[(line - size):(line - 1), ] <-
      paper[(line + 1):(line + size), ][size:1, ] |
      paper[(line - size):(line - 1), ]

    # Discard rows representing folded paper
    paper <- paper[1:(line - 1), ]
  }
}

# Print dots in a friendly format
paper <- ifelse(paper, "#", ".")
for (i in seq_len(nrow(paper))) {
  cat(paper[i, ])
  cat("\n")
}

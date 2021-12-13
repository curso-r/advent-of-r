# You reach another volcanically active part of the cave. It would be nice if
# you could do some kind of thermal imaging so you could tell ahead of time
# which caves are too hot to safely enter.
#
# Fortunately, the submarine seems to be equipped with a thermal camera! When
# you activate it, you are greeted with:
#
# Congratulations on your purchase! To activate this infrared thermal imaging
# camera system, please enter the code found on page 1 of the manual.
#
# Apparently, the Elves have never used this feature. To your surprise, you
# manage to find the manual; as you go to open it, page 1 falls out. It's a
# large sheet of transparent paper! The transparent paper is marked with random
# dots and includes instructions on how to fold it up (your puzzle input). For
# example:
#
# 6,10
# 0,14
# 9,10
# 0,3
# 10,4
# 4,11
# 6,0
# 6,12
# 4,1
# 0,13
# 10,12
# 3,4
# 3,0
# 8,4
# 1,10
# 2,14
# 8,10
# 9,0
#
# fold along y=7
# fold along x=5
#
# The first section is a list of dots on the transparent paper. 0,0 represents
# the top-left coordinate. The first value, x, increases to the right. The
# second value, y, increases downward. So, the coordinate 3,0 is to the right of
# 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form
# the following pattern, where # is a dot on the paper and . is an empty,
# unmarked position:
#
# ...#..#..#.
# ....#......
# ...........
# #..........
# ...#....#.#
# ...........
# ...........
# ...........
# ...........
# ...........
# .#....#.##.
# ....#......
# ......#...#
# #..........
# #.#........
#
# Then, there is a list of fold instructions. Each instruction indicates a line
# on the transparent paper and wants you to fold the paper up (for horizontal
# y=... lines) or left (for vertical x=... lines). In this example, the first
# fold instruction is fold along y=7, which designates the line formed by all of
# the positions where y is 7 (marked here with -):
#
# ...#..#..#.
# ....#......
# ...........
# #..........
# ...#....#.#
# ...........
# ...........
# -----------
# ...........
# ...........
# .#....#.##.
# ....#......
# ......#...#
# #..........
# #.#........
#
# Because this is a horizontal line, fold the bottom half up. Some of the dots
# might end up overlapping after the fold is complete, but dots will never
# appear exactly on a fold line. The result of doing this fold looks like this:
#
# #.##..#..#.
# #...#......
# ......#...#
# #...#......
# .#.#..#.###
# ...........
# ...........
#
# Now, only 17 dots are visible.
#
# Notice, for example, the two dots in the bottom left corner before the
# transparent paper is folded; after the fold is complete, those dots appear in
# the top left corner (at 0,0 and 0,1). Because the paper is transparent, the
# dot just below them in the result (at 0,3) remains visible, as it can be seen
# through the transparent paper.
#
# Also notice that some dots can end up overlapping; in this case, the dots
# merge together and become a single dot.
#
# The second fold instruction is fold along x=5, which indicates this line:
#
# #.##.|#..#.
# #...#|.....
# .....|#...#
# #...#|.....
# .#.#.|#.###
# .....|.....
# .....|.....
#
# Because this is a vertical line, fold left:
#
# #####
# #...#
# #...#
# #...#
# #####
# .....
# .....
#
# The instructions made a square!
#
# The transparent paper is pretty big, so for now, focus on just completing the
# first fold. After the first fold in the example above, 17 dots are visible -
# dots that end up overlapping after the fold is completed count as a single
# dot.
#
# How many dots are visible after completing just the first fold instruction on
# your transparent paper?

# Read table of where the dots are
dots <- "data-raw/13a_transparent_origami.txt" |>
  readr::read_lines() |>
  stringr::str_subset("^[0-9]") |>
  tibble::tibble() |>
  purrr::set_names("dot") |>
  tidyr::separate(dot, c("x", "y"), ",") |>
  dplyr::mutate_all(as.integer) |>
  dplyr::mutate_all(`+`, 1L)

# Read folding instructions
instructions <- "data-raw/13a_transparent_origami.txt" |>
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

# Only run first instruction
for (i in 1) {

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

    # Get rows above the fold, flip them and AND them with top side
    paper[(line - size):(line - 1), ] <-
      paper[(line + 1):(line + size), ][size:1, ] |
      paper[(line - size):(line - 1), ]

    # Discard rows representing folded paper
    paper <- paper[1:(line - 1), ]
  }
}

# Count dots on paper
sum(paper)

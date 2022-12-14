# --- Day 14: Regolith Reservoir ---
#
# The distress signal leads you to a giant waterfall! Actually, hang on -
# the signal seems like it's coming from the waterfall itself, and that
# doesn't make any sense. However, you do notice a little path that leads
# *behind* the waterfall.
#
# Correction: the distress signal leads you behind a giant waterfall!
# There seems to be a large cave system here, and the signal definitely
# leads further inside.
#
# As you begin to make your way deeper underground, you feel the ground
# rumble for a moment. Sand begins pouring into the cave! If you don't
# quickly figure out where the sand is going, you could quickly become
# trapped!
#
# Fortunately, your [familiarity](/2018/day/17) with analyzing the path of
# falling material will come in handy here. You scan a two-dimensional
# vertical slice of the cave above you (your puzzle input) and discover
# that it is mostly *air* with structures made of *rock*.
#
# Your scan traces the path of each solid rock structure and reports the
# `x,y` coordinates that form the shape of the path, where `x` represents
# distance to the right and `y` represents distance down. Each path
# appears as a single line of text in your scan. After the first point of
# each path, each point indicates the end of a straight horizontal or
# vertical line to be drawn from the previous point. For example:
#
#     498,4 -> 498,6 -> 496,6
#     503,4 -> 502,4 -> 502,9 -> 494,9
#
# This scan means that there are two paths of rock; the first path
# consists of two straight lines, and the second path consists of three
# straight lines. (Specifically, the first path consists of a line of rock
# from `498,4` through `498,6` and another line of rock from `498,6`
# through `496,6`.)
#
# The sand is pouring into the cave from point `500,0`.
#
# Drawing rock as `#`, air as `.`, and the source of the sand as `+`, this
# becomes:
#
#       4     5  5
#       9     0  0
#       4     0  3
#     0 ......+...
#     1 ..........
#     2 ..........
#     3 ..........
#     4 ....#...##
#     5 ....#...#.
#     6 ..###...#.
#     7 ........#.
#     8 ........#.
#     9 #########.
#
# Sand is produced *one unit at a time*, and the next unit of sand is not
# produced until the previous unit of sand *comes to rest*. A unit of sand
# is large enough to fill one tile of air in your scan.
#
# A unit of sand always falls *down one step* if possible. If the tile
# immediately below is blocked (by rock or sand), the unit of sand
# attempts to instead move diagonally *one step down and to the left*. If
# that tile is blocked, the unit of sand attempts to instead move
# diagonally *one step down and to the right*. Sand keeps moving as long
# as it is able to do so, at each step trying to move down, then
# down-left, then down-right. If all three possible destinations are
# blocked, the unit of sand *comes to rest* and no longer moves, at which
# point the next unit of sand is created back at the source.
#
# So, drawing sand that has come to rest as `o`, the first unit of sand
# simply falls straight down and then stops:
#
#     ......+...
#     ..........
#     ..........
#     ..........
#     ....#...##
#     ....#...#.
#     ..###...#.
#     ........#.
#     ......o.#.
#     #########.
#
# The second unit of sand then falls straight down, lands on the first
# one, and then comes to rest to its left:
#
#     ......+...
#     ..........
#     ..........
#     ..........
#     ....#...##
#     ....#...#.
#     ..###...#.
#     ........#.
#     .....oo.#.
#     #########.
#
# After a total of five units of sand have come to rest, they form this
# pattern:
#
#     ......+...
#     ..........
#     ..........
#     ..........
#     ....#...##
#     ....#...#.
#     ..###...#.
#     ......o.#.
#     ....oooo#.
#     #########.
#
# After a total of 22 units of sand:
#
#     ......+...
#     ..........
#     ......o...
#     .....ooo..
#     ....#ooo##
#     ....#ooo#.
#     ..###ooo#.
#     ....oooo#.
#     ...ooooo#.
#     #########.
#
# Finally, only two more units of sand can possibly come to rest:
#
#     ......+...
#     ..........
#     ......o...
#     .....ooo..
#     ....#ooo##
#     ...o#ooo#.
#     ..###ooo#.
#     ....oooo#.
#     .o.ooooo#.
#     #########.
#
# Once all *`24`* units of sand shown above have come to rest, all further
# sand flows out the bottom, falling into the endless void. Just for fun,
# the path any new sand takes before falling forever is shown here with
# `~`:
#
#     .......+...
#     .......~...
#     ......~o...
#     .....~ooo..
#     ....~#ooo##
#     ...~o#ooo#.
#     ..~###ooo#.
#     ..~..oooo#.
#     .~o.ooooo#.
#     ~#########.
#     ~..........
#     ~..........
#     ~..........
#
# Using your scan, simulate the falling sand. *How many units of sand come
# to rest before sand starts flowing into the abyss below?*

# Your input can be found on the file below:
input <- "2022/14_regolith_reservoir/input.txt"

safe_seq <- purrr::possibly(seq, NA_integer_)

df <- input |>
  readr::read_lines(skip_empty_rows = TRUE) |>
  stringr::str_split(" -> ") |>
  purrr::map(stringr::str_split, ",") |>
  purrr::map(purrr::map, purrr::set_names, "x", "y") |>
  purrr::map(purrr::transpose) |>
  purrr::map(tibble::as_tibble) |>
  purrr::map(tidyr::unnest, dplyr::everything()) |>
  purrr::map_dfr(dplyr::mutate_all, as.integer, .id = "path") |>
  dplyr::mutate(path = as.integer(path)) |>
  dplyr::group_by(path) |>
  dplyr::mutate(point = seq_len(dplyr::n()), .after = 1) |>
  dplyr::mutate(
    all_x = purrr::map2(x, dplyr::lead(x), safe_seq),
    all_y = purrr::map2(y, dplyr::lead(y), safe_seq)
  ) |>
  tidyr::unnest(c(all_x, all_y)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(all_x) | !is.na(all_y)) |>
  dplyr::select(all_x, all_y) |>
  dplyr::distinct()

offset_x <- - min(df$all_x) + 1 # Ignore left and right
offset_y <- + 1                 # They start at row 0
cave <- matrix(".", max(df$all_y) + offset_y, max(df$all_x) + offset_x)

# Debugging
print_mat <- function(mat) {
  for (i in seq_len(nrow(mat))) {
    cat(paste0(mat[i, ], collapse = ""))
    cat("\n")
  }
}

for (i in seq_len(nrow(df))) {
  cave[df$all_y[i] + offset_y, df$all_x[i] + offset_x] <- "#"
}

over <- FALSE
count <- 0
while (!over) {

  sand <- list(y = 0 + offset_y, x = 500 + offset_x)
  count <- count + 1

  while (TRUE) {

    if (sand$y + 1 > nrow(cave)) {
      over <- TRUE
      count <- count - 1
      break()
    } else if (cave[sand$y + 1, sand$x] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x
      next()
    }

    if (sand$x - 1 < 1) {
      over <- TRUE
      count <- count - 1
      break()
    } else if (cave[sand$y + 1, sand$x - 1] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x - 1
      next()
    }

    if (sand$x + 1 > ncol(cave)) {
      over <- TRUE
      count <- count - 1
      break()
    } else if (cave[sand$y + 1, sand$x + 1] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x + 1
      next()
    }

    cave[sand$y, sand$x] <- "o"
    break()

  }

}

count

# Once you're done with part 1, run the following line to fetch part 2:
aor::day_continue("2022-12-14", "2022/14_regolith_reservoir/puzzle.R")

# --- Part Two ---
#
# You realize you misread the scan. There isn't an endless void^[Endless
# Void is my C cover band.] at the bottom of the scan - there's floor,
# and you're standing on it!
#
# You don't have time to scan the floor, so assume the floor is an
# infinite horizontal line with a `y` coordinate equal to *two plus the
# highest `y` coordinate* of any point in your scan.
#
# In the example above, the highest `y` coordinate of any point is `9`,
# and so the floor is at `y=11`. (This is as if your scan contained one
# extra rock path like `-infinity,11 -> infinity,11`.) With the added
# floor, the example above now looks like this:
#
#             ...........+........
#             ....................
#             ....................
#             ....................
#             .........#...##.....
#             .........#...#......
#             .......###...#......
#             .............#......
#             .............#......
#             .....#########......
#             ....................
#     <-- etc #################### etc -->
#
# To find somewhere safe to stand, you'll need to simulate falling sand
# until a unit of sand comes to rest at `500,0`, blocking the source
# entirely and stopping the flow of sand into the cave. In the example
# above, the situation finally looks like this after *`93`* units of sand
# come to rest:
#
#     ............o............
#     ...........ooo...........
#     ..........ooooo..........
#     .........ooooooo.........
#     ........oo#ooo##o........
#     .......ooo#ooo#ooo.......
#     ......oo###ooo#oooo......
#     .....oooo.oooo#ooooo.....
#     ....oooooooooo#oooooo....
#     ...ooo#########ooooooo...
#     ..ooooo.......ooooooooo..
#     #########################
#
# Using your scan, simulate the falling sand until the source of the sand
# becomes blocked. *How many units of sand come to rest?*

df <- input |>
  readr::read_lines(skip_empty_rows = TRUE) |>
  stringr::str_split(" -> ") |>
  purrr::map(stringr::str_split, ",") |>
  purrr::map(purrr::map, purrr::set_names, "x", "y") |>
  purrr::map(purrr::transpose) |>
  purrr::map(tibble::as_tibble) |>
  purrr::map(tidyr::unnest, dplyr::everything()) |>
  purrr::map_dfr(dplyr::mutate_all, as.integer, .id = "path") |>
  dplyr::mutate(path = as.integer(path)) |>
  dplyr::group_by(path) |>
  dplyr::mutate(point = seq_len(dplyr::n()), .after = 1) |>
  dplyr::mutate(
    all_x = purrr::map2(x, dplyr::lead(x), safe_seq),
    all_y = purrr::map2(y, dplyr::lead(y), safe_seq)
  ) |>
  tidyr::unnest(c(all_x, all_y)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(all_x) | !is.na(all_y)) |>
  dplyr::select(all_x, all_y) |>
  dplyr::distinct()

offset_x <- - min(df$all_x) + 1 # Ignore left and right
offset_y <- + 1                 # They start at row 0
cave <- matrix(".", max(df$all_y + 2) + offset_y, max(df$all_x) + offset_x)

for (i in seq_len(nrow(df))) {
  cave[df$all_y[i] + offset_y, df$all_x[i] + offset_x] <- "#"
}

# Floor
cave[max(df$all_y + 2) + offset_y, ] <- "#"

over <- FALSE
count <- 0
while (!over) {

  sand <- list(y = 0 + offset_y, x = 500 + offset_x)
  count <- count + 1

  while (TRUE) {

    if (cave[sand$y + 1, sand$x] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x
      next()
    }

    if (sand$x - 1 < 1) { # Grow left

      offset_x <- offset_x + 1
      tmp_cave <- matrix(".", nrow(cave), ncol(cave) + 1)

      tmp_cave[, 2:ncol(tmp_cave)] <- cave

      tmp_cave[nrow(tmp_cave), 1] <- "#"
      tmp_cave[nrow(tmp_cave) - 1, 1] <- "o"

      cave <- tmp_cave
      rm(tmp_cave)

      break()

    } else if (cave[sand$y + 1, sand$x - 1] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x - 1
      next()
    }

    if (sand$x + 1 > ncol(cave)) { # Grow right

      tmp_cave <- matrix(".", nrow(cave), ncol(cave) + 1)

      tmp_cave[, 1:ncol(cave)] <- cave

      tmp_cave[nrow(tmp_cave), ncol(tmp_cave)] <- "#"
      tmp_cave[nrow(tmp_cave) - 1, ncol(tmp_cave)] <- "o"

      cave <- tmp_cave
      rm(tmp_cave)

      break()

    } else if (cave[sand$y + 1, sand$x + 1] == ".") {
      sand$y <- sand$y + 1
      sand$x <- sand$x + 1
      next()
    }

    if (sand$y == offset_y) {
      over <- TRUE
      break()
    } else {
      cave[sand$y, sand$x] <- "o"
      break()
    }

  }

}

count

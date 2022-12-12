# --- Day 12: Hill Climbing Algorithm ---
#
# You try contacting the Elves using your <span
# title="When you look up the specs for your handheld device, every field just says &quot;plot&quot;.">handheld
# device</span>, but the river you're following must be too low to get a
# decent signal.
#
# You ask the device for a heightmap of the surrounding area (your puzzle
# input). The heightmap shows the local area from above broken into a
# grid; the elevation of each square of the grid is given by a single
# lowercase letter, where `a` is the lowest elevation, `b` is the
# next-lowest, and so on up to the highest elevation, `z`.
#
# Also included on the heightmap are marks for your current position (`S`)
# and the location that should get the best signal (`E`). Your current
# position (`S`) has elevation `a`, and the location that should get the
# best signal (`E`) has elevation `z`.
#
# You'd like to reach `E`, but to save energy, you should do it in *as few
# steps as possible*. During each step, you can move exactly one square
# up, down, left, or right. To avoid needing to get out your climbing
# gear, the elevation of the destination square can be *at most one
# higher* than the elevation of your current square; that is, if your
# current elevation is `m`, you could step to elevation `n`, but not to
# elevation `o`. (This also means that the elevation of the destination
# square can be much lower than the elevation of your current square.)
#
# For example:
#
#     Sabqponm
#     abcryxxl
#     accszExk
#     acctuvwj
#     abdefghi
#
# Here, you start in the top-left corner; your goal is near the middle.
# You could start by moving down or right, but eventually you'll need to
# head toward the `e` at the bottom. From there, you can spiral around to
# the goal:
#
#     v..v<<<<
#     >v.vv<<^
#     .>vv>E^^
#     ..v>>>^^
#     ..>>>>>^
#
# In the above diagram, the symbols indicate whether the path exits each
# square moving up (`^`), down (`v`), left (`<`), or right (`>`). The
# location that should get the best signal is still `E`, and `.` marks
# unvisited squares.
#
# This path reaches the goal in *`31`* steps, the fewest possible.
#
# *What is the fewest steps required to move from your current position to
# the location that should get the best signal?*

# Your input can be found on the file below:
input <- "2022/12_hill_climbing_algorithm/input.txt"

data <- readr::read_lines(input, skip_empty_rows = TRUE)

cols <- stringr::str_length(data[1])
rows <- length(data)

heights <- data |>
  stringr::str_split("(?<=.)") |>
  purrr::map(stringr::str_subset, "[a-zA-Z]") |>
  purrr::flatten() |>
  purrr::flatten_chr() |>
  matrix(nrow = rows, ncol = cols, byrow = TRUE)

cost <- function(x, y) {
  if (x == "S") x <- "a"
  if (y == "S") y <- "a"
  if (x == "E") x <- "z"
  if (y == "E") y <- "z"

  x <- which(letters == x)
  y <- which(letters == y)

  if (x > y) return(1)
  else if (x == y) return(1)
  else y - x
}

id <- function(i, j) {
  paste0(stringr::str_pad(i, 2, pad = "0"), stringr::str_pad(j, 2, pad = "0"))
}

df <- tibble::tibble(from = character(0), to = character(0), cost = numeric(0))

for (i in seq_len(rows)) {
  for (j in seq_len(cols)) {

    # Up
    if (i > 1) {
      diff <- cost(heights[i, j], heights[i - 1, j])
      df <- tibble::add_row(df, from = id(i, j), to = id(i - 1, j), cost = diff)
    }

    # Down
    if (i < rows) {
      diff <- cost(heights[i, j], heights[i + 1, j])
      df <- tibble::add_row(df, from = id(i, j), to = id(i + 1, j), cost = diff)
    }

    # Left
    if (j > 1) {
      diff <- cost(heights[i, j], heights[i, j - 1])
      df <- tibble::add_row(df, from = id(i, j), to = id(i, j - 1), cost = diff)
    }

    # Right
    if (j < cols) {
      diff <- cost(heights[i, j], heights[i, j + 1])
      df <- tibble::add_row(df, from = id(i, j), to = id(i, j + 1), cost = diff)
    }

  }
}

start <- id(
  which(heights == "S", arr.ind = TRUE)[1],
  which(heights == "S", arr.ind = TRUE)[2]
)
end <- id(
  which(heights == "E", arr.ind = TRUE)[1],
  which(heights == "E", arr.ind = TRUE)[2]
)

df |>
  dplyr::filter(cost < 2) |>
  cppRouting::makegraph(directed = TRUE) |>
  cppRouting::get_distance_pair(from = start, to = end)

# Once you're done with part 1, run the following line to fetch part 2:
aor::day_continue("2022-12-12", "2022/12_hill_climbing_algorithm/puzzle.R")

# --- Part Two ---
#
# As you walk up the hill, you suspect that the Elves will want to turn
# this into a hiking trail. The beginning isn't very scenic, though;
# perhaps you can find a better starting point.
#
# To maximize exercise while hiking, the trail should start as low as
# possible: elevation `a`. The goal is still the square marked `E`.
# However, the trail should still be direct, taking the fewest steps to
# reach its goal. So, you'll need to find the shortest path from *any
# square at elevation `a`* to the square marked `E`.
#
# Again consider the example from above:
#
#     Sabqponm
#     abcryxxl
#     accszExk
#     acctuvwj
#     abdefghi
#
# Now, there are six choices for starting position (five marked `a`, plus
# the square marked `S` that counts as being at elevation `a`). If you
# start at the bottom-left square, you can reach the goal most quickly:
#
#     ...v<<<<
#     ...vv<<^
#     ...v>E^^
#     .>v>>>^^
#     >^>>>>>^
#
# This path reaches the goal in only *`29`* steps, the fewest possible.
#
# *What is the fewest steps required to move starting from any square with
# elevation `a` to the location that should get the best signal?*

end <- id(
  which(heights == "E", arr.ind = TRUE)[1],
  which(heights == "E", arr.ind = TRUE)[2]
)

pairs <- heights |>
  magrittr::equals("a") |>
  which(arr.ind = TRUE) |>
  tibble::as_tibble() |>
  dplyr::mutate(id = purrr::map2_chr(row, col, id)) |>
  dplyr::pull(id) |>
  purrr::map(c, end) |>
  purrr::transpose() |>
  purrr::map(purrr::flatten_chr)

df |>
  dplyr::filter(cost < 2) |>
  cppRouting::makegraph(directed = TRUE) |>
  cppRouting::get_distance_pair(from = pairs[[1]], to = pairs[[2]]) |>
  min(na.rm = TRUE)

# Next, you need to find the largest basins so you know what areas are most
# important to avoid.
#
# A basin is all locations that eventually flow downward to a single low point.
# Therefore, every low point has a basin, although some basins are very small.
# Locations of height 9 do not count as being in any basin, and all other
# locations will always be part of exactly one basin.
#
# The size of a basin is the number of locations within the basin, including the
# low point. The example above has four basins.
#
# The top-left basin, size 3:
#
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
#
# The top-right basin, size 9:
#
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
#
# The middle basin, size 14:
#
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
#
# The bottom-right basin, size 9:
#
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
#
# Find the three largest basins and multiply their sizes together. In the above
# example, this is 9 * 14 * 9 = 1134.
#
# What do you get if you multiply together the sizes of the three largest
# basins?

# Read heightmap as matrix and add padding
height <- "2021/data-raw/09a_smoke_basin.txt" |>
  readr::read_lines() |>
  stringr::str_split("") |>
  purrr::flatten_chr() |>
  as.integer() |>
  matrix(nrow = 100, ncol = 100, byrow = TRUE) |>
  rbind(rep(9, 100)) |>
  {\(m) rbind(rep(9, 100), m)}() |>
  cbind(rep(9, 102)) |>
  {\(m) cbind(rep(9, 102), m)}()

# Create a table of all points to explore
points <- purrr::cross2(2:101, 2:101) |>
  purrr::map(purrr::flatten_int) |>
  purrr::transpose() |>
  purrr::set_names("i", "j") |>
  tibble::as_tibble() |>
  tidyr::unnest(c(i, j))

# Explore one basin
explore <- function(a, b) {

  # Skip if point has been explored
  if (nrow(dplyr::filter(points, i == a, j == b)) == 0) return(0)

  # Mark point as explored
  points <<- dplyr::filter(points, i != a | j != b)

  # If height is 9, it doesn't belong in basin
  if (height[a, b] == 9) return(0)

  # Add neighbouring points to basin
  return(
    explore(a - 1, b) +
    explore(a + 1, b) +
    explore(a, b - 1) +
    explore(a, b + 1) + 1
  )
}

# Iterate over every point
basins <- matrix(rep(0, 10404), 102, 102)
for (i in 2:101) {
  for (j in 2:101) {
    basins[i, j] <- explore(i, j)
  }
}

# Multiply 3 largest basins
basins |>
  sort(decreasing = TRUE) |>
  magrittr::extract(1:3) |>
  prod()

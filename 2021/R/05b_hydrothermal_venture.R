# Unfortunately, considering only horizontal and vertical lines doesn't give you
# the full picture; you need to also consider diagonal lines.
#
# Because of the limits of the hydrothermal vent mapping system, the lines in
# your list will only ever be horizontal, vertical, or a diagonal line at
# exactly 45 degrees. In other words:
#
#   - An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
#   - An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
#
# Considering all lines from the above example would now produce the following
# diagram:
#
# 1.1....11.
# .111...2..
# ..2.1.111.
# ...1.2.2..
# .112313211
# ...1.2....
# ..1...1...
# .1.....1..
# 1.......1.
# 222111....
#
# You still need to determine the number of points where at least two lines
# overlap. In the above example, this is still anywhere in the diagram with a 2
# or larger - now a total of 12 points.
#
# Consider all of the lines. At how many points do at least two lines overlap?

# Expand coords, and count overlaps
"2021/data-raw/05b_hydrothermal_venture.txt" |>
  readr::read_csv(col_names = c("x1", "y1x2", "y2")) |>
  tidyr::separate(sep = " -> ", col = "y1x2", into = c("y1", "x2")) |>
  dplyr::mutate(
    dif_x = purrr::map2(x1, x2, seq),
    dif_y = purrr::map2(y1, y2, seq),
    coord = purrr::map2(dif_x, dif_y, paste)
  ) |>
  tidyr::unnest(coord) |>
  dplyr::count(coord) |>
  dplyr::filter(n > 1) |>
  nrow()

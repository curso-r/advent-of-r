# Maybe a fancy trick shot isn't the best idea; after all, you only have one
# probe, so you had better not miss.
#
# To get the best idea of what your options are for launching the probe, you
# need to find every initial velocity that causes the probe to eventually be
# within the target area after any step.
#
# In the above example, there are 112 different initial velocity values that
# meet these criteria:
#
# 23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
# 25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
# 8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
# 26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
# 20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
# 25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
# 25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
# 8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
# 24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
# 7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
# 23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
# 27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
# 8,-2    27,-8   30,-5   24,-7
#
# How many distinct initial velocity values cause the probe to be within the
# target area after any step?

# Read target as a data frame of coordinates
target <- "data-raw/17b_trick_shot.txt" |>
  readr::read_lines() |>
  stringr::str_split("[=,]") |>
  purrr::pluck(1) |>
  stringr::str_subset("^[0-9-]") |>
  stringr::str_replace("\\.\\.", ":") |>
  purrr::map(~eval(parse(text = .x))) |>
  purrr::cross() |>
  purrr::transpose() |>
  purrr::set_names("x", "y") |>
  tibble::as_tibble() |>
  tidyr::unnest(c(x, y))

# All possible combinarions of x and y velocities
vels <- purrr::cross(list(
  1:max(target$x),
  min(target$y):abs(min(target$y))
))

# Check pairs of velocities that work count them
n_works <- 0
for (vel in vels) {

  # Starting position
  x_pos <- 0
  y_pos <- 0

  # Starting velocities
  x_vel <- vel[[1]]
  y_vel <- vel[[2]]

  # Find max height for this pair of velocities
  max_height_ <- 0
  while (y_pos >= min(target$y) && x_pos <= max(target$x)) {

    # Update positions
    x_pos <- x_pos + x_vel
    y_pos <- y_pos + y_vel

    # If this pair of vels hits the target, update global max height
    if (x_pos %in% target$x && y_pos %in% target$y) {
      n_works <- n_works + 1
      break
    }

    # Update velocities
    x_vel <- if (x_vel > 0) x_vel - 1 else 0
    y_vel <- y_vel - 1
  }
}

# Return number of velocities that work
n_works

# You finally decode the Elves' message. HI, the message says. You continue
# searching for the sleigh keys.
#
# Ahead of you is what appears to be a large ocean trench. Could the keys have
# fallen into it? You'd better send a probe to investigate.
#
# The probe launcher on your submarine can fire the probe with any integer
# velocity in the x (forward) and y (upward, or downward if negative)
# directions. For example, an initial x,y velocity like 0,10 would fire the
# probe straight up, while an initial velocity like 10,-1 would fire the probe
# forward at a slight downward angle.
#
# The probe's x,y position starts at 0,0. Then, it will follow some trajectory
# by moving in steps. On each step, these changes occur in the following order:
#
#   - The probe's x position increases by its x velocity.
#   - The probe's y position increases by its y velocity.
#   - Due to drag, the probe's x velocity changes by 1 toward the value 0; that
#     is, it decreases by 1 if it is greater than 0, increases by 1 if it is
#     less than 0, or does not change if it is already 0.
#   - Due to gravity, the probe's y velocity decreases by 1.
#
# For the probe to successfully make it into the trench, the probe must be on
# some trajectory that causes it to be within a target area after any step. The
# submarine computer has already calculated this target area (your puzzle
# input). For example:
#
# target area: x=20..30, y=-10..-5
#
# This target area means that you need to find initial x,y velocity values such
# that after any step, the probe's x position is at least 20 and at most 30, and
# the probe's y position is at least -10 and at most -5.
#
# Given this target area, one initial velocity that causes the probe to be
# within the target area after any step is 7,2:
#
# .............#....#............
# .......#..............#........
# ...............................
# S........................#.....
# ...............................
# ...............................
# ...........................#...
# ...............................
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTT#TT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
#
# In this diagram, S is the probe's initial position, 0,0. The x coordinate
# increases to the right, and the y coordinate increases upward. In the bottom
# right, positions that are within the target area are shown as T. After each
# step (until the target area is reached), the position of the probe is marked
# with #. (The bottom-right # is both a position the probe reaches and a
# position in the target area.)
#
# Another initial velocity that causes the probe to be within the target area
# after any step is 6,3:
#
# ...............#..#............
# ...........#........#..........
# ...............................
# ......#..............#.........
# ...............................
# ...............................
# S....................#.........
# ...............................
# ...............................
# ...............................
# .....................#.........
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................T#TTTTTTTTT
# ....................TTTTTTTTTTT
#
# Another one is 9,0:
#
# S........#.....................
# .................#.............
# ...............................
# ........................#......
# ...............................
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTT#
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
# ....................TTTTTTTTTTT
#
# One initial velocity that doesn't cause the probe to be within the target area
# after any step is 17,-4:
#
# S..............................................................
# ...............................................................
# ...............................................................
# ...............................................................
# .................#.............................................
# ....................TTTTTTTTTTT................................
# ....................TTTTTTTTTTT................................
# ....................TTTTTTTTTTT................................
# ....................TTTTTTTTTTT................................
# ....................TTTTTTTTTTT..#.............................
# ....................TTTTTTTTTTT................................
# ...............................................................
# ...............................................................
# ...............................................................
# ...............................................................
# ................................................#..............
# ...............................................................
# ...............................................................
# ...............................................................
# ...............................................................
# ...............................................................
# ...............................................................
# ..............................................................#
#
# The probe appears to pass through the target area, but is never within it
# after any step. Instead, it continues down and to the right - only the first
# few steps are shown.
#
# If you're going to fire a highly scientific probe out of a super cool probe
# launcher, you might as well do it with style. How high can you make the probe
# go while still reaching the target area?
#
# In the above example, using an initial velocity of 6,9 is the best you can do,
# causing the probe to reach a maximum y position of 45. (Any higher initial y
# velocity causes the probe to overshoot the target area entirely.)
#
# Find the initial velocity that causes the probe to reach the highest y
# position and still eventually be within the target area after any step. What
# is the highest y position it reaches on this trajectory?

# Read target as a data frame of coordinates
target <- "data-raw/17a_trick_shot.txt" |>
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

# Check pairs of velocities that work and get their max heights
max_height <- 0
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

    # Update local max height
    if (y_pos > max_height_) max_height_ <- y_pos

    # If this pair of vels hits the target, update global max height
    if (x_pos %in% target$x && y_pos %in% target$y) {
      if (max_height_ > max_height) max_height <- max_height_
    }

    # Update velocities
    x_vel <- if (x_vel > 0) x_vel - 1 else 0
    y_vel <- y_vel - 1
  }
}

# Return global max height
max_height

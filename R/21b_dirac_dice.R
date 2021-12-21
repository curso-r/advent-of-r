# Now that you're warmed up, it's time to play the real game.
#
# A second compartment opens, this time labeled Dirac dice. Out of it falls a
# single three-sided die.
#
# As you experiment with the die, you feel a little strange. An informational
# brochure in the compartment explains that this is a quantum die: when you roll
# it, the universe splits into multiple copies, one copy for each possible
# outcome of the die. In this case, rolling the die always splits the universe
# into three copies: one where the outcome of the roll was 1, one where it was
# 2, and one where it was 3.
#
# The game is played the same as before, although to prevent things from getting
# too far out of hand, the game now ends when either player's score reaches at
# least 21.
#
# Using the same starting positions as in the example above, player 1 wins in
# 444356092776315 universes, while player 2 merely wins in 341960390180808
# universes.
#
# Using your given starting positions, determine every possible outcome. Find
# the player that wins in more universes; in how many universes does that player
# win?

# Read starting positions
pos <- "data-raw/21b_dirac_dice.txt" |>
  readr::read_lines() |>
  stringr::str_extract("[0-9]+$") |>
  as.numeric()

# Assign positions
p1_pos <- pos[1]
p2_pos <- pos[2]

# Take mod for dice (no 0s)
die_mod <- function(e1, e2) ((e1 - 1) %% e2) + 1

# Create an ID for `states`
id <- function(a, b, c, d) paste0(a, ",", b, ",", c, ",", d)

# Count wins on every state of the game
states <- list()
count_states <- function(p1_pos, p2_pos, p1_pts = 0, p2_pts = 0) {
  this_id <- id(p1_pos, p2_pos, p1_pts, p2_pts)

  # Halting conditions
  if (p1_pts >= 21) return(c(1, 0))
  if (p2_pts >= 21) return(c(0, 1))
  if (this_id %in% names(states)) return(states[[this_id]])

  # All possible combinations
  rolls <- list(1:3, 1:3, 1:3) |>
    purrr::cross() |>
    purrr::map(purrr::flatten_int) |>
    purrr::map_int(sum)

  # Iterate over rolls and recurse to next states
  wins_total <- c(0, 0)
  for (roll in rolls) {
    p1_pos_ <- die_mod(p1_pos + roll, 10)

    # Go on to next states and add wins to counter
    wins <- count_states(p2_pos, p1_pos_, p2_pts, p1_pts + p1_pos_)
    wins_total <- wins_total + rev(wins)
  }

  # Update states and return
  states[[this_id]] <<- wins_total
  return(wins_total)
}

# Run dynamic programming
count_states(p1_pos, p2_pos) |>
  max() |>
  format(scientific = FALSE)

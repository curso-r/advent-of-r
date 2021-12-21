
# Read starting positions
pos <- "data-raw/21a_dirac_dice.txt" |>
  readr::read_lines() |>
  stringr::str_extract("[0-9]+$") |>
  as.numeric()

# Assign positions
p1_pos <- pos[1]
p2_pos <- pos[2]

# Assign scores
p1_pts <- 0
p2_pts <- 0

# Take mod for dice (no 0s)
die_mod <- function(e1, e2) ((e1 - 1) %% e2) + 1

# Start loop with die on 1
die <- 1
counter <- 0
while (TRUE) {

  # P1 rolls 3 times
  p1_rolls <- die:(die + 2)
  p1_rolls <- die_mod(p1_rolls, 100)

  # Update die state and roll counter
  die <- die_mod(p1_rolls[3] + 1, 100)
  counter <- counter + 3

  # Update P1 score
  p1_pos <- p1_pos + sum(p1_rolls)
  p1_pos <- die_mod(p1_pos, 10)
  p1_pts <- p1_pts + p1_pos

  # Stop if P1 has won
  if (p1_pts >= 1000) break

  # P1 rolls 3 times
  p2_rolls <- die:(die + 2)
  p2_rolls <- die_mod(p2_rolls, 100)

  # Update die state and roll counter
  die <- die_mod(p2_rolls[3] + 1, 100)
  counter <- counter + 3

  # Update P2 score
  p2_pos <- p2_pos + sum(p2_rolls)
  p2_pos <- die_mod(p2_pos, 10)
  p2_pts <- p2_pts + p2_pos

  # Stop if P2 has won
  if (p2_pts >= 1000) break
}

# Counter * loosing player's score
min(p1_pts, p2_pts) * counter

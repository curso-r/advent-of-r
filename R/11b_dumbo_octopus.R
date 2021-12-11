# It seems like the individual flashes aren't bright enough to navigate.
# However, you might have a better option: the flashes seem to be synchronizing!
#
# In the example above, the first time all octopuses flash simultaneously is
# step 195:
#
# After step 193:
# 5877777777
# 8877777777
# 7777777777
# 7777777777
# 7777777777
# 7777777777
# 7777777777
# 7777777777
# 7777777777
# 7777777777
#
# After step 194:
# 6988888888
# 9988888888
# 8888888888
# 8888888888
# 8888888888
# 8888888888
# 8888888888
# 8888888888
# 8888888888
# 8888888888
#
# After step 195:
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
# 0000000000
#
# If you can calculate the exact moments when the octopuses will all flash
# simultaneously, you should be able to navigate through the cavern. What is the
# first step during which all octopuses flash?

# Read grid as matrix
dumbo <- "data-raw/11b_dumbo_octopus.txt" |>
  readr::read_table(col_names = FALSE) |>
  tidyr::separate(X1, paste0("C", 0:10), "") |>
  dplyr::select(-C0) |>
  dplyr::mutate_all(as.numeric) |>
  as.matrix()

# Iterate over 100 steps
for (k in 1:1000) {
  print(k)

  # Increase energy levels
  dumbo <- (dumbo + 1) %% 10

  # Add energy to octopi that recieved flashes
  flag <- FALSE
  while(!flag) {

    # Add energy to octopi adjecent to flashes
    dumbo_ <- dumbo
    for (i in 1:10) {
      for (j in 1:10) {

        # Indexes of "splash zone"
        i1 <- i - 1
        i2 <- min(i + 1, 10)
        j1 <- j - 1
        j2 <- min(j + 1, 10)

        # Add energy to window (except center)
        if (dumbo[i, j] == 0) {
          dumbo_[i1:i2, j1:j2] <- dumbo_[i1:i2, j1:j2] + 1
          dumbo_[i, j] <- dumbo_[i, j] - 1
        }
      }
    }

    # Separate old flashes from the ones from the last iteration
    dumbo <- ifelse(dumbo == -1, 0, dumbo)

    # Override flashed octopi with 0 (they can't get energy after flashing)
    dumbo <- ifelse(dumbo == 0, 0, dumbo_)

    # Check whether step is over
    if (!any(dumbo > 9)) {
      flag <- TRUE
    } else {

      # Prevent old flashes from being counted and mark recent flashes
      dumbo <- ifelse(dumbo == 0, -1, dumbo)
      dumbo <- ifelse(dumbo > 9, 0, dumbo)
    }
  }

  # Stop if all octopi flashed
  if (all(dumbo %in% c(0, -1))) {
    break()
  }
}

# Print
k

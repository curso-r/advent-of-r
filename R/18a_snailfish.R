
# Find position where a number needs exploding
find_explode <- function(num) {
  chrs <- stringr::str_split(num, "")[[1]]

  # Iterate over chrs to find a deep pair
  counter <- 0
  for (i in seq_along(chrs)) {
    if (chrs[i] == "[") {
      counter <- counter + 1
    } else if (chrs[i] == "]") {
      counter <- counter - 1

      # If pair is deep, return
      if (counter >= 4) {

        # Get start of pair
        len <- num |>
          stringr::str_sub(end = i) |>
          stringr::str_extract("\\[[^\\[]*?$") |>
          stringr::str_length() |>
          magrittr::subtract(1)

        # Return boundaries of pair
        return(c(i - len, i))
      }
    }
  }

  # Otherwise, return NULL
  return(NULL)
}

# Run explode algorithm
explode <- function(num) {

  # Find pair to explode
  pos <- find_explode(num)

  # If there is no pair, return number
  if (is.null(pos)) return(num)

  # Extract numbers from pair
  pair <- num |>
    stringr::str_sub(pos[1], pos[2]) |>
    stringr::str_extract_all("[0-9]+") |>
    purrr::pluck(1) |>
    as.numeric()

  # Get lefthand side of num, ending at the pair to explode
  lhs <- stringr::str_sub(num, end = pos[1] - 1)

  # Find rightmost number in lhs and add pair[1]
  left_num <- lhs |>
    stringr::str_extract("[0-9]+(?=[^0-9]+$)") |>
    as.numeric() |>
    magrittr::add(pair[1])

  # Get righthand side of num, starting at the pair to explode
  rhs <- stringr::str_sub(num, pos[2] + 1)

  # Find leftmost number in rhs and add pair[2]
  right_num <- rhs |>
    stringr::str_extract("^[^0-9]+[0-9]+") |>
    stringr::str_remove("^[^0-9]+") |>
    as.numeric() |>
    magrittr::add(pair[2])

  # Replace numbers with their new values
  lhs <- stringr::str_replace(lhs, "[0-9]+([^0-9]+)$", paste0(left_num, "\\1"))
  rhs <- stringr::str_replace(rhs, "^([^0-9]+)[0-9]+", paste0("\\1", right_num))

  # Paste num back together
  return(paste0(lhs, "0", rhs))
}

# Run split algorithm
split <- function(num) {

  # Check if number should be split
  if (!stringr::str_detect(num, "[0-9]{2,}")) return(num)

  # Create pair from number larger than 9
  pair <- num |>
    stringr::str_extract("[0-9]{2,}") |>
    as.numeric() |>
    {\(n) paste0("[", floor(n / 2), ",", ceiling(n / 2), "]")}()

  # Replace number with pair
  stringr::str_replace(num, "[0-9]{2,}", pair)
}

# Run snailfish addition
snailfish_sum <- function(num1, num2) {

  # Paste numbers as a pair
  num <- paste0("[", num1, ",", num2, "]")

  # Apply explode() and split() until number doesn't change
  num_ <- ""
  while (num_ != num) {
    num_ <- num

    # Explode and, if something happened, go back
    num <- explode(num)
    if (num_ != num) next

    # Split
    num <- split(num)
  }

  return(num)
}

# Do one round of the magnitude algorithm
get_one_magnitude <- function(num) {

  # Get magnitude of leftmost pair
  val <- num |>
    stringr::str_extract("\\[[^\\[\\]]+\\]") |>
    stringr::str_extract_all("[0-9]+") |>
    purrr::pluck(1) |>
    as.numeric() |>
    {\(n) 3 * n[1] + 2 * n[2]}() |>
    as.character()

  # Replace value in the right position
  stringr::str_replace(num, "\\[[^\\[\\]]+\\]", val)
}

# Run magnitude algorithm
get_magnitude <- function(num) {

  # While there are still pairs, get one magnitude
  while (stringr::str_detect(num, "\\[")) {
    num <- get_one_magnitude(num)
  }

  # Return magnitude as numeric
  return(as.numeric(num))
}

# Reduce list of numbers with snalfish addition and get magnitude
"data-raw/18a_snailfish.txt" |>
  readr::read_lines() |>
  purrr::reduce(snailfish_sum) |>
  get_magnitude()

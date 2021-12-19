# You notice a second question on the back of the homework assignment:
#
# What is the largest magnitude you can get from adding only two of the
# snailfish numbers?
#
# Note that snailfish addition is not commutative - that is, x + y and y + x can
# produce different results.
#
# Again considering the last example homework assignment above:
#
# [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
# [[[5,[2,8]],4],[5,[[9,9],0]]]
# [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
# [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
# [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
# [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
# [[[[5,4],[7,7]],8],[[8,3],8]]
# [[9,3],[[9,9],[6,[4,9]]]]
# [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
# [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
#
# The largest magnitude of the sum of any two snailfish numbers in this list is
# 3993. This is the magnitude of [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]] +
# [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]], which reduces to
# [[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]].
#
# What is the largest magnitude of any sum of two different snailfish numbers
# from the homework assignment?

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

# Cross numbers with themselves and try every combination
"data-raw/18b_snailfish.txt" |>
  readr::read_lines() |>
  {\(ns) list(ns, ns)}() |>
  purrr::cross(`==`) |>
  purrr::map_dbl(~get_magnitude(snailfish_sum(.x[[1]], .x[[2]]))) |>
  max()

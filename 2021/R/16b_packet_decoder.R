# Now that you have the structure of your transmission decoded, you can
# calculate the value of the expression it represents.
#
# Literal values (type ID 4) represent a single number as described above. The
# remaining type IDs are more interesting:
#
#   - Packets with type ID 0 are sum packets - their value is the sum of the
#     values of their sub-packets. If they only have a single sub-packet, their
#     value is the value of the sub-packet.
#   - Packets with type ID 1 are product packets - their value is the result of
#     multiplying together the values of their sub-packets. If they only have a
#     single sub-packet, their value is the value of the sub-packet.
#   - Packets with type ID 2 are minimum packets - their value is the minimum of
#     the values of their sub-packets.
#   - Packets with type ID 3 are maximum packets - their value is the maximum of
#     the values of their sub-packets.
#   - Packets with type ID 5 are greater than packets - their value is 1 if the
#     value of the first sub-packet is greater than the value of the second
#     sub-packet; otherwise, their value is 0. These packets always have exactly
#     two sub-packets.
#   - Packets with type ID 6 are less than packets - their value is 1 if the
#     value of the first sub-packet is less than the value of the second
#     sub-packet; otherwise, their value is 0. These packets always have exactly
#     two sub-packets.
#   - Packets with type ID 7 are equal to packets - their value is 1 if the
#     value of the first sub-packet is equal to the value of the second
#     sub-packet; otherwise, their value is 0. These packets always have exactly
#     two sub-packets.
#
# Using these rules, you can now work out the value of the outermost packet in
# your BITS transmission.
#
# For example:
#
#   - C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
#   - 04005AC33890 finds the product of 6 and 9, resulting in the value 54.
#   - 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
#   - CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
#   - D8005AC2A8F0 produces 1, because 5 is less than 15.
#   - F600BC2D8F produces 0, because 5 is not greater than 15.
#   - 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
#   - 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
#
# What do you get if you evaluate the expression represented by your
# hexadecimal-encoded BITS transmission?

# Alternative to strtoi(, base = 2) that doesn't overflow
strton <- function(x) {
  y <- as.numeric(strsplit(x, "")[[1]])
  sum(y * 2^rev((seq_along(y) - 1)))
}

# Convert hex string to bit binary string
hex_to_bits <- function(hex) {
  hex |>
    stringr::str_split("") |>
    purrr::pluck(1) |>
    purrr::map(~paste(rev(as.integer(intToBits(strtoi(.x, 16)))))) |>
    purrr::map(magrittr::extract, 29:32) |>
    purrr::flatten_chr() |>
    stringr::str_c(collapse = "")
}

# Get a packet's version
get_version <- function(pkt) {
  strton(stringr::str_sub(pkt, 1, 3))
}

# Get a packet's type
get_type <- function(pkt) {
  strton(stringr::str_sub(pkt, 4, 6))
}

# Get value of a literal packet
get_literal <- function(pkt) {
  interval <- c(7, 11)

  # Iterate util last group is found
  literal <- ""
  flag <- FALSE
  while (!flag) {

    # Get group specified by interval
    group <- stringr::str_sub(pkt, interval[1], interval[2])
    literal <- stringr::str_c(literal, stringr::str_sub(group, 2))

    # Stop if it's last group, otherwise move the interval
    if (!as.integer(stringr::str_sub(group, 1, 1))) {
      flag <- TRUE
    } else {
      interval <- interval + 5
    }
  }

  # Return structure describing packet
  return(list(
    type = get_type(pkt),
    len = interval[2],
    value = strton(literal)
  ))
}

# Get value of an operator packet
get_operator <- function(pkt) {
  indicator <- stringr::str_sub(pkt, 7, 7)

  # Initialize structure
  out <- list(
    type = get_type(pkt)
  )

  # Handle different indicators
  if (as.integer(indicator)) {

    # Get number of sub-packets and separate rest of the packet
    num <- strton(stringr::str_sub(pkt, 8, 18))
    rest <- stringr::str_sub(pkt, 19)
    out$len <- 18

    # Iterate over `num` packets
    for (i in seq_len(num)) {

      # Get literal or get operator
      sub <- if (get_type(rest) == 4) get_literal(rest) else get_operator(rest)
      out$len <- out$len + sub$len
      out <- c(out, list(sub))

      # Update rest of the packet given last sub-packet's length
      rest <- stringr::str_sub(rest, sub$len + 1)
    }
  } else {

    # Get length limit of sub-packets and separate rest of the packet
    lim <- strton(stringr::str_sub(pkt, 8, 22))
    rest <- stringr::str_sub(pkt, 23)
    out$len <- 22

    # Iterate while sub-packets haven't reached the limit
    while (lim > 0) {

      # Get literal or get operator
      sub <- if (get_type(rest) == 4) get_literal(rest) else get_operator(rest)
      out$len <- out$len + sub$len
      out <- c(out, list(sub))

      # Update rest of the packet given last sub-packet's length
      rest <- stringr::str_sub(rest, sub$len + 1)
      lim <- lim - sub$len
    }
  }

  return(out)
}

# Evaluate a packet tree
get_value <- function(tree) {

  # Functions corresponding to packet types
  fun <- switch(as.character(tree$type),
    "0" = sum,
    "1" = prod,
    "2" = min,
    "3" = max,
    "5" = `>`,
    "6" = `<`,
    "7" = `==`,
  )

  # Apply function to sub-packets
  apply_fun <- function(tree) {
    tree |>
      purrr::keep(names(tree) == "") |>
      purrr::map(get_value) |>
      purrr::reduce(fun)
  }

  # Apply recursively
  if (tree$type == 4) tree$value else as.numeric(apply_fun(tree))
}

# Decode a hex packet's value
decode <- function(hex) {
  pkt <- hex_to_bits(hex)
  tree <- if (get_type(pkt) == 4) get_literal(pkt) else get_operator(pkt)

  get_value(tree)
}

# Read packet as hex and get value
"2021/data-raw/16b_packet_decoder.txt" |>
  readr::read_lines() |>
  decode() |>
  format(scientific = FALSE)

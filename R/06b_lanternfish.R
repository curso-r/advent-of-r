# Suppose the lanternfish live forever and have unlimited food and space. Would
# they take over the entire ocean?
#
# After 256 days in the example above, there would be a total of 26984457539
# lanternfish!
#
# How many lanternfish would there be after 256 days?

# Run n cycles of lanternfish reproduction
reproduce <- function(fish, n = 80) {

  # Halting condition
  if (n == 0) return(sum(fish$n))

  # Reduce internal timers
  fish <- dplyr::mutate(fish, timer = timer - 1L)

  # Create new fish
  babies <- fish |>
    dplyr::filter(timer == -1L) |>
    dplyr::mutate(timer = 8L)

  # Reset timers and recurse
  fish |>
    dplyr::bind_rows(babies) |>
    dplyr::mutate(timer = ifelse(timer == -1L, 6L, timer)) |>
    dplyr::group_by(timer) |>
    dplyr::summarise(n = sum(n)) |>
    reproduce(n = n - 1)
}

# Read list of fish and reproduce for 80 days
"data-raw/06b_lanternfish.txt" |>
  readr::read_lines() |>
  stringr::str_split(",") |>
  purrr::pluck(1) |>
  as.integer() |>
  tibble::as_tibble() |>
  purrr::set_names("timer") |>
  dplyr::count(timer) |>
  reproduce(n = 256) |>
  format(scientific = FALSE)

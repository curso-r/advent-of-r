# You've almost reached the exit of the cave, but the walls are getting closer
# together. Your submarine can barely still fit, though; the main problem is
# that the walls of the cave are covered in chitons, and it would be best not to
# bump any of them.
#
# The cavern is large, but has a very low ceiling, restricting your motion to
# two dimensions. The shape of the cavern resembles a square; a quick scan of
# chiton density produces a map of risk level throughout the cave (your puzzle
# input). For example:
#
# 1163751742
# 1381373672
# 2136511328
# 3694931569
# 7463417111
# 1319128137
# 1359912421
# 3125421639
# 1293138521
# 2311944581
#
# You start in the top left position, your destination is the bottom right
# position, and you cannot move diagonally. The number at each position is its
# risk level; to determine the total risk of an entire path, add up the risk
# levels of each position you enter (that is, don't count the risk level of your
# starting position unless you enter it; leaving it adds no risk to your total).
#
# Your goal is to find a path with the lowest total risk. In this example, a
# path with the lowest total risk is highlighted here:
#
# 1163751742
# 1381373672
# 2136511328
# 3694931569
# 7463417111
# 1319128137
# 1359912421
# 3125421639
# 1293138521
# 2311944581
#
# The total risk of this path is 40 (the starting position is never entered, so
# its risk is not counted).
#
# What is the lowest total risk of any path from the top left to the bottom
# right?

# Read cave risks as matrix
cave <- "data-raw/15a_chiton.txt" |>
  readr::read_lines() |>
  stringr::str_split("") |>
  purrr::flatten_chr() |>
  as.integer() |>
  matrix(100, 100, byrow = TRUE)

# Create data frame with cost between coords
graph <- tibble::tibble()
for (i in 1:prod(dim(cave))) {

  vals <- c()
  if (i %% 100 != 0)  vals <- append(vals, i + 1L)
  if (i %% 100 != 1)  vals <- append(vals, i - 1L)
  if (i > 100)        vals <- append(vals, i - 100L)
  if (i < 9901)       vals <- append(vals, i + 100L)

  node <- tibble::tibble(from_vertex = i, to_vertex = vals, cost = cave[vals])
  graph <- dplyr::bind_rows(graph, node)
}

# Create graph and run Dijkstra's
path <- graph |>
  cppRouting::makegraph(directed = TRUE) |>
  cppRouting::get_path_pair(from = 1L, to = 10000L) |>
  purrr::pluck(1) |>
  as.integer()

# Calculate path's total risk
graph |>
  dplyr::filter(to_vertex %in% path) |>
  dplyr::group_by(to_vertex) |>
  dplyr::summarise(cost = cost[1]) |>
  dplyr::summarise(risk = sum(cost)) |>
  dplyr::pull(risk) |>
  magrittr::subtract(cave[1])

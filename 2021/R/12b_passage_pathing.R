# After reviewing the available paths, you realize you might have time to visit
# a single small cave twice. Specifically, big caves can be visited any number
# of times, a single small cave can be visited at most twice, and the remaining
# small caves can be visited at most once. However, the caves named start and
# end can only be visited exactly once each: once you leave the start cave, you
# may not return to it, and once you reach the end cave, the path must end
# immediately.
#
# Now, the 36 possible paths through the first example above are:
#
# start,A,b,A,b,A,c,A,end
# start,A,b,A,b,A,end
# start,A,b,A,b,end
# start,A,b,A,c,A,b,A,end
# start,A,b,A,c,A,b,end
# start,A,b,A,c,A,c,A,end
# start,A,b,A,c,A,end
# start,A,b,A,end
# start,A,b,d,b,A,c,A,end
# start,A,b,d,b,A,end
# start,A,b,d,b,end
# start,A,b,end
# start,A,c,A,b,A,b,A,end
# start,A,c,A,b,A,b,end
# start,A,c,A,b,A,c,A,end
# start,A,c,A,b,A,end
# start,A,c,A,b,d,b,A,end
# start,A,c,A,b,d,b,end
# start,A,c,A,b,end
# start,A,c,A,c,A,b,A,end
# start,A,c,A,c,A,b,end
# start,A,c,A,c,A,end
# start,A,c,A,end
# start,A,end
# start,b,A,b,A,c,A,end
# start,b,A,b,A,end
# start,b,A,b,end
# start,b,A,c,A,b,A,end
# start,b,A,c,A,b,end
# start,b,A,c,A,c,A,end
# start,b,A,c,A,end
# start,b,A,end
# start,b,d,b,A,c,A,end
# start,b,d,b,A,end
# start,b,d,b,end
# start,b,end
#
# The slightly larger example above now has 103 paths through it, and the even
# larger example now has 3509 paths through it.
#
# Given these new rules, how many paths through this cave system are there?

# Get all distinct paths in graph
all_paths <- list()
get_paths <- function(graph, path = "start", boost = FALSE) {

  # Check if current node is "small"
  cave <- tail(path, 1)
  is_small <- stringr::str_to_lower(cave) == cave

  # Halting conditions
  if (cave == "end") {all_paths <<- append(all_paths, list(path)); return(1)}
  if (!any(graph$orig == cave)) return(0)

  # Find next nodes to search
  searches <- graph |>
    dplyr::filter(orig == cave) |>
    dplyr::pull(dest) |>
    purrr::map(purrr::prepend, path)

  # Update available nodes
  graph_ <- if (is_small) dplyr::filter(graph, orig != cave) else graph

  # Iterate over possible paths
  for (search in searches) {
    get_paths(graph_, search, boost = boost)

    # An option is not removing node from graph and boosting
    if (!boost && is_small && cave != "start") {
      get_paths(graph, search, boost = TRUE)
    }
  }

  # Return global list
  return(all_paths)
}

# Read paths as graph and count unique
"2021/data-raw/12b_passage_pathing.txt" |>
  readr::read_table(col_names = "path") |>
  tidyr::separate(path, c("orig", "dest"), "-") |>
  {\(d) dplyr::bind_rows(d, purrr::set_names(d, rev(names(d))))}() |>
  dplyr::filter(dest != "start", orig != "end") |>
  get_paths() |>
  purrr::map_chr(stringr::str_c, collapse = "|") |>
  unique() |>
  length()

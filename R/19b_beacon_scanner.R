# Sometimes, it's a good idea to appreciate just how big the ocean is. Using the
# Manhattan distance, how far apart do the scanners get?
#
# In the above example, scanners 2 (1105,-1205,1229) and 3 (-92,-2380,-20) are
# the largest Manhattan distance apart. In total, they are 1197 + 1175 + 1249 =
# 3621 units apart.
#
# What is the largest Manhattan distance between any two scanners?

# Convert c(x,y,z) to "x,y,z"
vec_to_str <- function(vec) {
  stringr::str_c(vec, collapse = ",")
}

# Convert "x,y,z" to c(x,y,z)
str_to_vec <- function(str) {
  as.integer(stringr::str_split(str, ",")[[1]])
}

# Shorcut for choose(n,2) of a list
choose_pairs <- function(l) {
  seq_along(l) |>
    list(seq_along(l)) |>
    purrr::cross(`==`) |>
    purrr::transpose() |>
    purrr::map(purrr::flatten_int) |>
    purrr::set_names("a", "b") |>
    dplyr::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(ordered = paste0(sort(c(a, b)), collapse = ",")) |>
    dplyr::group_by(ordered) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-ordered) |>
    dplyr::mutate(
      a = purrr::map(a, ~l[[.x]]),
      b = purrr::map(b, ~l[[.x]])
    )
}

# Apply all 24 rotations to a point
apply_rotations <- function(point) {
  rotations <- list(
    list(c(-1, 0, 0), c(0, -1, 0), c(0, 0, 1)),
    list(c(-1, 0, 0), c(0, 0, -1), c(0, -1, 0)),
    list(c(-1, 0, 0), c(0, 0, 1), c(0, 1, 0)),
    list(c(-1, 0, 0), c(0, 1, 0), c(0, 0, -1)),
    list(c(0, -1, 0), c(-1, 0, 0), c(0, 0, -1)),
    list(c(0, -1, 0), c(0, 0, -1), c(1, 0, 0)),
    list(c(0, -1, 0), c(0, 0, 1), c(-1, 0, 0)),
    list(c(0, -1, 0), c(1, 0, 0), c(0, 0, 1)),
    list(c(0, 0, -1), c(-1, 0, 0), c(0, 1, 0)),
    list(c(0, 0, -1), c(0, -1, 0), c(-1, 0, 0)),
    list(c(0, 0, -1), c(0, 1, 0), c(1, 0, 0)),
    list(c(0, 0, -1), c(1, 0, 0), c(0, -1, 0)),
    list(c(0, 0, 1), c(-1, 0, 0), c(0, -1, 0)),
    list(c(0, 0, 1), c(0, -1, 0), c(1, 0, 0)),
    list(c(0, 0, 1), c(0, 1, 0), c(-1, 0, 0)),
    list(c(0, 0, 1), c(1, 0, 0), c(0, 1, 0)),
    list(c(0, 1, 0), c(-1, 0, 0), c(0, 0, 1)),
    list(c(0, 1, 0), c(0, 0, -1), c(-1, 0, 0)),
    list(c(0, 1, 0), c(0, 0, 1), c(1, 0, 0)),
    list(c(0, 1, 0), c(1, 0, 0), c(0, 0, -1)),
    list(c(1, 0, 0), c(0, -1, 0), c(0, 0, -1)),
    list(c(1, 0, 0), c(0, 0, -1), c(0, 1, 0)),
    list(c(1, 0, 0), c(0, 0, 1), c(0, -1, 0)),
    list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  )

  # Create a data frame with (x, y, z) of rotations and a rotation ID
  rotations |>
    purrr::map(purrr::map, `*`, point) |>
    purrr::map(purrr::map, sum) |>
    purrr::map(purrr::flatten_dbl) |>
    dplyr::tibble() |>
    purrr::set_names("point") |>
    dplyr::mutate(rotation = rotations) |>
    tibble::rowid_to_column() |>
    tidyr::unnest(point) |>
    dplyr::mutate(coord = rep(c("x", "y", "z"), dplyr::n() / 3)) |>
    tidyr::pivot_wider(names_from = coord, values_from = point) |>
    dplyr::mutate(rotation = purrr::map_chr(rotation, paste, collapse = ",")) |>
    dplyr::select(x, y, z, rotation)
}

# Function factory for transforming a point with rotation + translation
# Input should be a row from apply_rotations() output
factory_transform <- function(df) {

  # Extract rotation operation from df
  rot <- df$rotation |>
    stringr::str_split("c\\(") |>
    purrr::pluck(1) |>
    stringr::str_remove("\\),?") |>
    stringr::str_subset(",") |>
    stringr::str_split(", ") |>
    purrr::map(as.numeric)

  # Extract translation operation from df
  trans <- c(df$dif_x, df$dif_y, df$dif_z)

  # Return function that applies full transformation
  function(vec) {
    rot |>
      purrr::map(`*`, vec) |>
      purrr::map(sum) |>
      purrr::flatten_dbl() |>
      magrittr::add(trans)
  }
}

# Get all intersections of different scanners
get_intersections <- function(points) {

  # Pair up scanners and check return their intersections
  points |>
    purrr::map(choose_pairs) |>
    purrr::map(
      dplyr::mutate, # Intersections are based on distances between points
      dist = purrr::map2_dbl(a, b, ~sum((.x - .y)**2))
    ) |>
    choose_pairs() |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::map(~dplyr::inner_join(.x[["a"]][[1]], .x[["b"]][[1]], "dist")) |>
    purrr::keep(~nrow(.x) >= 66) # 66 = C(12, 2) = 12 points in intersec.
}

# Get all transforms that might convert pairs1 into pairs2
get_transforms <- function(pairs1, pairs2) {

  # Create functions that take pairs1[2] into pairs2[2a] or pairs2[2b]
  dplyr::bind_rows(
    dplyr::mutate(
      apply_rotations(pairs1$a.x[[2]]),
      ref_x = pairs2$a.y[[2]][1],
      ref_y = pairs2$a.y[[2]][2],
      ref_z = pairs2$a.y[[2]][3]
    ),
    dplyr::mutate(
      apply_rotations(pairs1$a.x[[2]]),
      ref_x = pairs2$b.y[[2]][1],
      ref_y = pairs2$b.y[[2]][2],
      ref_z = pairs2$b.y[[2]][3]
    )
  ) |>
    dplyr::mutate(
      dif_x = ref_x - x,
      dif_y = ref_y - y,
      dif_z = ref_z - z
    ) |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::map(factory_transform)
}

# Find correct transformation function
find_transform <- function(df, funs) {

  # Given transformation functions, find the one that converts more points from
  # df (set of intersections) correctly
  df |>
    tibble::rowid_to_column("pair_id") |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    purrr::map(~{
      .x |>
        dplyr::mutate(,
          fun_a.x = list(purrr::map(funs, ~.x(a.x[[1]]))),
          fun_id = list(seq_along(funs))
        ) |>
        tidyr::unnest(dplyr::starts_with("fun")) |>
        dplyr::select(-dist) |>
        tidyr::unnest(dplyr::everything())
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      a_works = a.y == fun_a.x,
      b_works = b.y == fun_a.x
    ) |>
    dplyr::group_by(pair_id, fun_id) |>
    dplyr::summarise(
      some_works = all(a_works) || all(b_works), .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(fun_id) |>
    dplyr::summarise(works = sum(some_works)) |>
    dplyr::slice_max(works) |>
    dplyr::pull(fun_id)
}

# Read points as a list of vectors
points <- "data-raw/19b_beacon_scanner.txt" |>
  readr::read_lines() |>
  tibble::tibble() |>
  purrr::set_names("point") |>
  dplyr::mutate(
    scanner = as.integer(stringr::str_detect(point, "scanner")),
    scanner = cumsum(scanner) - 1
  ) |>
  dplyr::filter(!stringr::str_detect(point, "scanner")) |>
  dplyr::filter(point != "") |>
  dplyr::group_split(scanner) |>
  purrr::map(dplyr::pull, point) |>
  purrr::map(purrr::map, str_to_vec)

# Reduce scanners into a single region, recording correct transformation funs
save_funs <- list()
while (length(points) > 1) {

  # Get one pair of scanners that have an intersection
  pairs <- get_intersections(points)[[1]]

  # Get all transformation functions
  funs <- get_transforms(
    dplyr::select(pairs, a.x, b.x),
    dplyr::select(pairs, a.y, b.y)
  )

  # Find correct transformation function
  transformation <- funs[[find_transform(pairs, funs)]]
  save_funs <- c(save_funs, transformation)

  # Convert points to strings
  pairs <- pairs |>
    dplyr::select(-dist) |>
    dplyr::mutate_all(purrr::map_chr, vec_to_str)

  # Create a copy of points that is also strings
  points_ <- purrr::map(points, purrr::map_chr, vec_to_str)

  # Find scanner that was used as reference by transformation()
  for (i in seq_along(points_)) {

    ref <- all(c(pairs$a.y, pairs$b.y) %in% points_[[i]])
    if (ref) reference <- i
  }

  # Find scanner that was transformed by transformation()
  for (i in seq_along(points_)) {

    trns <- all(c(pairs$a.x, pairs$b.x) %in% points_[[i]])
    if (trns) transformed <- i
  }

  # Actually apply transformation() to all points from scanner and add points
  # of transformed scanner to reference scanner
  points_[[reference]] <- points[[transformed]] |>
    purrr::map(transformation) |>
    purrr::map_chr(vec_to_str) |>
    c(points_[[reference]]) |>
    unique()

  # Update list of points
  points_[[transformed]] <- NULL
  points <- purrr::map(points_, purrr::map, str_to_vec)
}

# Apply saved transformations to scanner coordinates and take Manhattan dist
save_funs |>
  purrr::map(~.x(c(0, 0, 0))) |>
  choose_pairs() |>
  dplyr::mutate(dist = purrr::map2_dbl(a, b, ~sum(abs(.x - .y)))) |>
  dplyr::slice_max(dist) |>
  dplyr::pull(dist)

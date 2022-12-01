# You still can't quite make out the details in the image. Maybe you just didn't
# enhance it enough.
#
# If you enhance the starting input image in the above example a total of 50
# times, 3351 pixels are lit in the final output image.
#
# Start again with the original input image and apply the image enhancement
# algorithm 50 times. How many pixels are lit in the resulting image?

# Convert a 3x3 region of an image into an integer
img_to_int <- function(image) {

  # Flatten matrix into 1 row
  bits <- ifelse(image == ".", 0, 1)
  binary <- paste0(as.vector(t(bits)), collapse = "")

  # String to integer
  strtoi(binary, base = 2)
}

# Apply enhancement algorithm
enhance <- function(image, algo) {

  # Iterate over rows and columns
  new_image <- image
  for (i in 2:(nrow(image) - 1)) {
    for (j in 2:(ncol(image) - 1)) {

      # Replace [i,j] with corresponding index in algo
      ind <- img_to_int(image[(-1:1 + i), (-1:1 + j)])
      new_image[i, j] <- algo[ind + 1]
    }
  }

  # Trim padding and return
  new_image[2:(nrow(image) - 1), 2:(ncol(image) - 1)]
}

# Add padding
add_padding <- function(image) {

  # Add 2 rows to top and bottom
  image <- rbind(
    image[1, ], image[1, ],
    image,
    image[nrow(image), ], image[nrow(image), ]
  )

  # Add 2 cols to left and right
  image <- cbind(
    image[, 1], image[, 1],
    image,
    image[, ncol(image)], image[, ncol(image)]
  )

  return(image)
}

# Read enhancement algorithm as a string vector
algo <- "2021/data-raw/20b_trench_map.txt" |>
  readr::read_lines(n_max = 1) |>
  stringr::str_split("") |>
  purrr::pluck(1)

# Read image as a matrix (and add padding)
image <- "2021/data-raw/20b_trench_map.txt" |>
  readr::read_lines(skip = 2) |>
  purrr::prepend(rep(paste0(rep(".", 100), collapse = ""), 3)) |>
  append(rep(paste0(rep(".", 100), collapse = ""), 3)) |>
  {\(s) stringr::str_c("...", s, "...")}() |>
  stringr::str_split("") |>
  purrr::flatten_chr() |>
  matrix(106, 106, byrow = TRUE)

# Apply algo 50 times
image <- enhance(image, algo)
for (i in seq_len(49)) {
  image <- enhance(add_padding(image), algo)
}

# Count light pixels
image |>
  magrittr::equals("#") |>
  sum()

# https://adventofcode.com/2015/day/6

d6 <- function(day = 6, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example, " ")
  
  toggle <- sapply(input, \(x) x[1] == "toggle")
  on     <- sapply(input, \(x) x[2] == "on")
  off    <- sapply(input, \(x) x[2] == "off")
  coord1 <- 
    sapply(input, \(x) x[length(x)-2]) |> 
    strsplit(",") |> 
    lapply(\(x) as.integer(x) + 1) |> 
    do.call(what = rbind)
  coord2 <- 
    sapply(input, \(x) x[length(x)]) |> 
    strsplit(",") |> 
    lapply(\(x) as.integer(x) + 1) |> 
    do.call(what = rbind)
  
  if (part == 1) {
    # initially all off
    mat <- matrix(FALSE, nrow = 1000, ncol = 1000)
    for (i in seq_len(length(toggle))) {
      y_range <- coord1[i,1]:coord2[i,1]
      x_range <- coord1[i,2]:coord2[i,2]
      if (toggle[i]) {
        mat[y_range, x_range] <- !mat[y_range, x_range]
      } else if (on[i]) {
        mat[y_range, x_range] <- TRUE
      } else if (off[i]) {
        mat[y_range, x_range] <- FALSE
      }
    }
    return(sum(mat))
  }
  
  if (part == 2) {
    # initially all off
    mat <- matrix(0, nrow = 1000, ncol = 1000)
    for (i in seq_len(length(toggle))) {#
      y_range <- coord1[i,1]:coord2[i,1]
      x_range <- coord1[i,2]:coord2[i,2]
      if (toggle[i]) {
        mat[y_range, x_range] <- mat[y_range, x_range] + 2
      } else if (on[i]) {
        mat[y_range, x_range] <- mat[y_range, x_range] + 1
      } else if (off[i]) {
        mat[y_range, x_range] <- mat[y_range, x_range] - 1
        mat[mat < 0] <- 0
        
          # mat[y_range, x_range] |> 
          # apply(1, \(x) pmax(0, x - 1))
      }
    }
    return(sum(mat))
  }
}

# part 1
d6(example = TRUE,  part = 1) == 998996
d6(example = FALSE, part = 1) == 400410

# part 2
d6(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d6(example = FALSE, part = 1),
  d6(example = FALSE, part = 2)
)


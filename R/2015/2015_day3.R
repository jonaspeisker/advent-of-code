# https://adventofcode.com/2015/day/3

d3 <- function(day = 3, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example) |> unlist()
  up <- input == "^"
  down <- input == "v"
  left <- input == "<"
  right <- input == ">"

  
  if (part == 1) {
    x <- c(0, cumsum(right) - cumsum(left))
    y <- c(0, cumsum(up) - cumsum(down))
    return(paste0(x, ",", y) |> unique() |> length())
  }
  
  if (part == 2) {
    santa_ind <- seq(1, length(input), 2)
    x_santa <- c(0, cumsum(right[santa_ind]) - cumsum(left[santa_ind]))
    y_santa <- c(0, cumsum(up[santa_ind]) - cumsum(down[santa_ind]))
    coords_santa <- paste0(x_santa, ",", y_santa)
    
    x_robo_santa <- c(0, cumsum(right[-santa_ind]) - cumsum(left[-santa_ind]))
    y_robo_santa <- c(0, cumsum(up[-santa_ind]) - cumsum(down[-santa_ind]))
    coords_robo_santa <- paste0(x_robo_santa, ",", y_robo_santa)
    
    return(c(coords_santa, coords_robo_santa) |> unique() |> length())
  }
}

# part 1
d3(example = TRUE,  part = 1) == 4
d3(example = FALSE, part = 1) == 2592

# part 2
d3(example = TRUE,  part = 2) == 3
d3(example = FALSE, part = 2) == 2360

# benchmark
microbenchmark::microbenchmark(
  d3(example = FALSE, part = 1), # 13 ms
  d3(example = FALSE, part = 2)  # 14 ms
)


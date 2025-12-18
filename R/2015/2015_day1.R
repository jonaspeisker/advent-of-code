# https://adventofcode.com/2015/day/1

d1 <- function(day = 1, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example) |> unlist()
  # ( means he should go up one floor
  move <- ifelse(input == ")", -1, 1)
  floor <- cumsum(move)
  # To what floor do the instructions take Santa?
  if (part == 1) { # faster than Reduce(`+`, move)
    return(floor[length(floor)])
  }
  # What is the position that causes Santa to first enter the basement?
  if (part == 2) { 
    basement <- floor == -1L
    return(which(basement)[1])
  }
}

# part 1
d1(example = TRUE,  part = 1) == 3
d1(example = FALSE, part = 1) == 74

# part 2
d1(example = TRUE,  part = 2) == 1
d1(example = FALSE, part = 2) == 1795

# benchmark
microbenchmark::microbenchmark(
  d1(example = FALSE, part = 1), # 0.61 ms
  d1(example = FALSE, part = 2)  # 0.64 ms
)


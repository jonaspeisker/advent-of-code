# https://adventofcode.com/2015/day/11

d11 <- function(day = 11, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)


  if (part == 1) {
  
    return()
  }
  
  if (part == 2) {
  
    return()
  }
}

# part 1
d11(example = TRUE,  part = 1) ==
d11(example = FALSE, part = 1)

# part 2
d11(example = TRUE,  part = 2) ==
d11(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d11(example = FALSE, part = 1),
  d11(example = FALSE, part = 2)
)


# https://adventofcode.com/2015/day/24

d24 <- function(day = 24, year = 2015, example = TRUE, part = 1) {
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
d24(example = TRUE,  part = 1) ==
d24(example = FALSE, part = 1)

# part 2
d24(example = TRUE,  part = 2) ==
d24(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d24(example = FALSE, part = 1),
  d24(example = FALSE, part = 2)
)


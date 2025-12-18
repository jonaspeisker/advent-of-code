# https://adventofcode.com/2015/day/16

d16 <- function(day = 16, year = 2015, example = TRUE, part = 1) {
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
d16(example = TRUE,  part = 1) ==
d16(example = FALSE, part = 1)

# part 2
d16(example = TRUE,  part = 2) ==
d16(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d16(example = FALSE, part = 1),
  d16(example = FALSE, part = 2)
)


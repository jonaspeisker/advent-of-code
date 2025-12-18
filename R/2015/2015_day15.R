# https://adventofcode.com/2015/day/15

d15 <- function(day = 15, year = 2015, example = TRUE, part = 1) {
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
d15(example = TRUE,  part = 1) ==
d15(example = FALSE, part = 1)

# part 2
d15(example = TRUE,  part = 2) ==
d15(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d15(example = FALSE, part = 1),
  d15(example = FALSE, part = 2)
)


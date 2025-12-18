# https://adventofcode.com/2015/day/14

d14 <- function(day = 14, year = 2015, example = TRUE, part = 1) {
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
d14(example = TRUE,  part = 1) ==
d14(example = FALSE, part = 1)

# part 2
d14(example = TRUE,  part = 2) ==
d14(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d14(example = FALSE, part = 1),
  d14(example = FALSE, part = 2)
)


# https://adventofcode.com/2015/day/4

d4 <- function(day = 4, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)

  if (part == 1) {
    x <- 0
    while (!is_key) {
      x <- x + 1
      md5_tmp <- 
        paste0(input, x) |> 
        charToRaw() |> 
        tools::md5sum(bytes = _)
      is_key <- substr(md5_tmp, 1, 5) == "00000"
    }
    return(x)
  }
  
  if (part == 2) {
    x <- 0
    while (!is_key) {
      x <- x + 1
      md5_tmp <- 
        paste0(input, x) |> 
        charToRaw() |> 
        tools::md5sum(bytes = _)
      is_key <- substr(md5_tmp, 1, 6) == "000000"
    }
    return(x)
  }
}

# part 1
d4(example = TRUE,  part = 1) == 609043
d4(example = FALSE, part = 1) == 346386

# part 2
d4(example = FALSE, part = 2) == 9958218

# benchmark
microbenchmark::microbenchmark(
  d4(example = FALSE, part = 1),
  d4(example = FALSE, part = 2),
  times = 1
)


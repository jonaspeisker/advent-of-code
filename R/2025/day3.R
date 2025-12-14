# https://adventofcode.com/2025/day/3

d3 <- function(day = 3, year = 2025, example = TRUE, pick_n) {
  verbose <- example
  input <- read_split(day, year, example) # list of vectors
  
  # pick n batteries to produce the largest number
  max_joltage <- lapply(input, function(x){
    max <- NULL
    for (i in (pick_n-1):0) {
      # find index of first maximum, reserving the minimum number of digits 
      # at the end that are necessary to fill the remaining digits i
      x_len <- length(x)
      max_ind <- 
        x[1:(x_len-i)] |> 
        which.max()     
      max <- c(max, x[max_ind])     # append new digits
      x <- x[(max_ind+1):x_len] # continue to the right of max  
    }
    if (verbose) { message(max) }
    return(
      max |> paste0(collapse="") |> as.numeric()
      )
  }) 
  return(Reduce(sum, max_joltage))
}

# part 1
d3(example = TRUE, pick_n = 2) == 357
d3(example = FALSE, pick_n = 2) == 17694
# part 2
d3(example = TRUE, pick_n = 12) == 3121910778619
d3(example = FALSE, pick_n = 12) == 175659236361660
# benchmark
microbenchmark(
  d3(example = FALSE, pick_n = 2), #  5.7 ms
  d3(example = FALSE, pick_n = 12) # 11.6 ms
)

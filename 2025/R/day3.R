#### main ####
d3 <- function(day = 3, example = TRUE, batteries) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() |> 
    strsplit("") # list of vectors
  
  max_joltage <- lapply(input, function(x){
    max <- NULL
    for (i in (batteries-1):0) {
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
d3(example = TRUE, batteries = 2) == 357
d3(example = FALSE, batteries = 2) == 17694
# part 2
d3(example = TRUE, batteries = 12) == 3121910778619
d3(example = FALSE, batteries = 12) == 175659236361660
# benchmark
microbenchmark(
  d3(example = FALSE, batteries = 2), #  5.7 ms
  d3(example = FALSE, batteries = 12) # 11.6 ms
)

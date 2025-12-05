max_joltage <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day3/",
    batteries,
    verbose=FALSE
    ) {
  input <- readLines(paste0(path, file_name))
  split <- input |> strsplit("")                     # list of vectors
  max_joltage <- lapply(split, function(x){
    max <- NULL
    for (i in (batteries-1):0) {
      # find max, reserving the min number of digits at the end
      # that are necessary to fill the remaining digits
      max_ind <- x[1:(length(x)-i)] |> which.max()     
      max <- c(max, x[max_ind])                      # append
      x <- x[(max_ind+1):length(x)]                  # continue to the right of max  
    }
    if (verbose) { message(max) }
    return(
      max |> paste0(collapse="") |> as.numeric()
      )
  }) 
  joltage_sum <- Reduce(sum, max_joltage)
  message("Total joltage is ", joltage_sum)
}

# part 1
max_joltage("input_example.txt", batteries=2, verbose=T)
max_joltage("input.txt", batteries=2)
# part 2
max_joltage("input_example.txt", batteries=12, verbose=T)
max_joltage("input.txt", batteries=12)

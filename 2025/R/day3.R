#### main ####
d3 <- function(day=3, example=T, batteries) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() |> 
    strsplit("") # list of vectors
  
  max_joltage <- lapply(input, function(x){
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
d3(example=T, batteries=2)
d3(example=F, batteries=2)
# part 2
d3(example=T, batteries=12)
d3(example=F, batteries=12)

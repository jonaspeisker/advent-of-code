file_name <- "day3/input_example.txt"
largest_joltage <- function(file_name) {
  input <- read.table(file_name, colClasses = "character")[,1]
  split <- input |> strsplit("")
  max_joltage <- lapply(split, function(x){
    d1_ind <- x[-length(x)] |> which.max() # index of first max (not last)
    d2 <- x[(d1_ind+1):length(x)] |> max() # max of remaining digits
    max <- paste0(x[d1_ind], d2) |> as.integer() # paste back together
  }) 
  return(Reduce(sum, max_joltage))
}

largest_joltage("day3/input_example.txt")
largest_joltage("day3/input.txt")


x_ <- x[-x_len]                # first digits cannot be last
c1_ind <- which(x_ == max(x_)) # candidate indices of start values
c2 <- sapply(c1_ind, function(i){
  x[(i+1):x_len] |> max()      # max of remaining digits for each candidate
})


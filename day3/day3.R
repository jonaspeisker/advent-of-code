max_joltage <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day3/"
    ) {
  input <- readLines(paste0(path, file_name))
  split <- input |> strsplit("")
  max_joltage <- lapply(split, function(x){
    d1_ind <- x[-length(x)] |> which.max() # index of first max (not last)
    d2 <- x[(d1_ind+1):length(x)] |> max() # max of remaining digits
    max <- paste0(x[d1_ind], d2) |> as.integer() # paste back together
  }) 
  return(Reduce(sum, max_joltage))
}

max_joltage("input_example.txt")
max_joltage("input.txt")


x_ <- x[-x_len]                # first digits cannot be last
c1_ind <- which(x_ == max(x_)) # candidate indices of start values
c2 <- sapply(c1_ind, function(i){
  x[(i+1):x_len] |> max()      # max of remaining digits for each candidate
})


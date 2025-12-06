grand_total <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day6/",
    verbose=FALSE
) {
  input <- read.table(paste0(path, file_name))
  nr <- nrow(input) 
  
  # iterate over cols
  reduced <- sapply(seq_len(ncol(input)), function(i) {
    op <- match.fun(input[nr, i])             # last row contains operator
    Reduce(op, as.numeric(input[1:(nr-1),i])) # others rows contain numbers
  })
  
  grand_total <- sum(reduced)
  message("The grand total is ", grand_total)
}

grand_total("input_example.txt", verbose=T)
grand_total("input.txt")

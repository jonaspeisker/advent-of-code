beam_splits <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day7/"
) {
  input <- 
    readLines(paste0(path, file_name)) |> 
    strsplit("")
  
  # iterate over rows
  n_cols <- length(input[[1]])
  beam_prev <- input[[1]] == "S"
  split_counter <- 0
  beams <- beam_prev
  for (i in 2:length(input)) {
    # find splitting positions and count
    split_here <- (input[[i]] == "^") & beam_prev
    split_ind <- which(split_here)
    split_counter <- split_counter + length(split_ind)
    
    # get new beam locations
    beam_next <- beam_prev & !split_here
    beam_next[split_ind - 1] <- TRUE
    beam_next[split_ind + 1] <- TRUE
    
    # save for next iteration
    beam_prev <- beam_next
    beams <- rbind(beams, beam_prev)
  }
  
  message("The number of beam splits is ", split_counter)
  invisible(beams)
}

tree <- beam_splits("input_example.txt")
beam_splits("input.txt")

print_tree <- function(x) {
  ifelse(tree, "|", ".")
}
print_tree(tree)

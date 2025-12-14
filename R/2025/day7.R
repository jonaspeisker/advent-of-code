# https://adventofcode.com/2025/day/7

#### part 1 ####
# How many times will the beam be split?
d7p1 <- function(day = 7, year = 2025, example = TRUE) {
  verbose <- example
  input <- read_split(day, year, example)
  
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
  
  if (verbose) {
    print(ifelse(beams, "|", "."))
  }
  
  return(split_counter)
}

d7p1(example = TRUE) == 21
d7p1(example = FALSE) == 1546

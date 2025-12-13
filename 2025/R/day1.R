d1 <- function(
    day = 1, 
    example = TRUE, 
    start_pos = 50, 
    part = 1
  ) {
  stopifnot(part %in% 1:2, is.numeric(start_pos))
  verbose <- example
  turns <- # vector
    get_file_name(day, example) |> 
    readLines()
  dist_full <- turns |> substr(2, 100) |> as.integer() # distance
  dist_mod  <- dist_full %% 100 # remainder of division by 100
  turn_left <- substr(turns, 1, 1) == "L" # direction 
  dist_mod_incr <- ifelse(turn_left, -dist_mod, dist_mod) # subtract if left
  
  # get dial positions
  # cumulative position
  dial_pos_raw <- c(start_pos, start_pos + cumsum(dist_mod_incr)) 
  dial_pos <- dial_pos_raw %% 100   # dial positions in 0–99 range
  # to determine whether 0 or 100 was crossed
  turn_raw <- dial_pos[-length(dial_pos)] + dist_mod_incr 
  if (verbose) { cat("Dial positions:\n", dial_pos, "\n") }
  
  # count zeros
  is_zero <- dial_pos == 0              # is current position zero
  was_zero <- is_zero[-length(is_zero)] # was previous position zero
  if (part == 1) {
    return(sum(is_zero))
  }
  if (part == 2) {
    crosses_zero <- 
      ( turn_left & turn_raw < 0   & !was_zero & !is_zero[-1]) | 
      (!turn_left & turn_raw > 100 & !was_zero & !is_zero[-1])
    hundreds <- dist_full %/% 100 # number of full hundreds
    return(sum(is_zero, crosses_zero, hundreds))
  }
}

# part 1
d1(example = TRUE, part = 1) == 3
d1(example = FALSE, part = 1) == 1120
# part 2
d1(example = TRUE, part = 2) == 6
d1(example = FALSE, part = 2) == 6554

d1 <- function(
    day = 1, 
    example = TRUE, 
    start_pos = 50, 
    any_pass = FALSE
  ) {
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
  if (verbose) { cat("Dial positions:\n", dial_pos) }
  
  # count zeros
  is_zero <- dial_pos == 0              # is current position zero
  was_zero <- is_zero[-length(is_zero)] # was previous position zero
  if (!any_pass) {
    zeros <- sum(is_zero)
    message("The password is: ", zeros)
    return(zeros)
  } else {
    crosses_zero <- 
      ( turn_left & turn_raw < 0   & !was_zero & !is_zero[-1]) | 
      (!turn_left & turn_raw > 100 & !was_zero & !is_zero[-1])
    hundreds <- dist_full %/% 100 # number of full hundreds
    zeros <- sum(is_zero, crosses_zero, hundreds)
    message("The password is: ", zeros)
    return(zeros)
  }
}

# part 1
d1(example = TRUE, any_pass = FALSE) == 3
d1(example = FALSE, any_pass = FALSE) == 1120
# part 2
d1(example = TRUE, any_pass = TRUE) == 6
d1(example = FALSE, any_pass = TRUE) == 6554

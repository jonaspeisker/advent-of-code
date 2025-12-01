library(tidyverse)

get_password <- function(
    file, 
    print_info = FALSE
  ) {
  # read input
  df <- 
    read_csv(file, col_names = "turn", show_col_types = FALSE) |> 
    mutate(
      dir = substr(turn, 1, 1),
      dist = parse_number(turn)
    )
  
  # turn dial 
  counter <- 0
  position <- 50
  previous_position <- 50
  for (i in seq_along(df$turn)) {
    turns_mod <- df$dist[i] %% 100
    # times passed 0 (hundreds)
    zeros <- (df$dist[i] - turns_mod) / 100
    
    if (df$dir[i] == "L") {     # left
      pos_tmp <- position - turns_mod
      if (pos_tmp >= 0) {       # not cross 0
        position <- pos_tmp
      } else {                  # cross 0
        position <- pos_tmp + 100
        if (previous_position != 0) {
          zeros <- zeros + 1
        }
      }
    } else {                    # right
      pos_tmp <- position + turns_mod
      if (pos_tmp <= 99) {      # not cross 0
        position <- pos_tmp
      } else {                  # cross 0
        position <- pos_tmp - 100
        if (position != 0 & previous_position != 0) {
          zeros <- zeros + 1
        }
      }
    }
    
    zeros <- zeros + (position == 0) # current position 0
    if (print_info) {
      message(
        "Position after turn ", df$turn[i], ": ", position, 
        " (", zeros, " zeros)"
      )
    }
    
    counter <- counter + zeros
    previous_position <- position 
  }
  message("The password is ", counter)
}

get_password("day1/input_example.txt", print_info = TRUE)
get_password("day1/input.txt")

library(tidyverse)

get_password <- function(file) {
  df <- 
    read_csv(file, col_names = "turn", show_col_types = FALSE) |> 
    mutate(
      dir = substr(turn, 1, 1),
      dist = parse_number(turn)
    )
  
  counter <- 0
  position <- 50
  for (i in seq_along(df$turn)) {
    if (df$dir[i] == "L") {# left
      pos_tmp <- position - df$dist[i]
      if (pos_tmp >= 0) {# cross 0?
        position <- pos_tmp
      } else {
        position <- pos_tmp + 100
      }
    } else {# right
      pos_tmp <- position + df$dist[i]
      if (pos_tmp <= 99) {# cross 0?
        position <- pos_tmp
      } else {
        position <- pos_tmp - 100
      }
    }
    # message("Position after turn ", i, ": ", position) 
    
    counter <- counter + (position == 0)
  }
  message("The password is ", counter)
}

get_password("day1/input_example.txt")
get_password("day1/input.txt")

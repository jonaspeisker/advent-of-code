get_password <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day1/", 
    any_pass=F, 
    verbose=F
    ) {
  # read input
  df <- read.table(paste0(path, file_name), col.names="turn") 
  df$dir <- df$turn |> substr(1, 1)
  df$dist <- df$turn |> substr(2, 100) |> as.integer()
  
  # turn dial 
  counter <- 0
  position <- 50
  previous_position <- 50
  for (i in seq_along(df$turn)) {
    zeros <- 0
    turns_mod <- df$dist[i] %% 100
    # times passed 0 (hundreds)
    if (any_pass) {
      zeros <- (df$dist[i] - turns_mod) / 100
    }
    
    if (df$dir[i] == "L") {     # left
      pos_tmp <- position - turns_mod
      if (pos_tmp >= 0) {       # not cross 0
        position <- pos_tmp
      } else {                  # cross 0
        position <- pos_tmp + 100
        if (any_pass & previous_position != 0) {
          zeros <- zeros + 1
        }
      }
    } else {                    # right
      pos_tmp <- position + turns_mod
      if (pos_tmp <= 99) {      # not cross 0
        position <- pos_tmp
      } else {                  # cross 0
        position <- pos_tmp - 100
        if (any_pass & position != 0 & previous_position != 0) {
          zeros <- zeros + 1
        }
      }
    }
    # current position 0
    zeros <- zeros + (position == 0) 
    
    # add zeros of current turn
    counter <- counter + zeros
    previous_position <- position 
    if (verbose) {
      message(
        "Position after turn ", df$turn[i], ": ", position, 
        " (", zeros, " zeros)"
      )
    }
  }
  message("The password is ", counter)
}

# part 1
get_password("input_example.txt", any_pass=F, verbose=T)
get_password("input.txt", any_pass=F)
# part 2
get_password("input_example.txt", any_pass=T, verbose=T)
get_password("input.txt", any_pass=T)

#### main ####
d1 <- function(day=1, example=T, any_pass=F) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.table(col.names="turn")
  input$dir <- input$turn |> substr(1, 1) # direction
  input$dist <- input$turn |> substr(2, 100) |> as.integer() # distance
  
  # turn dial 
  counter <- 0
  position <- 50
  previous_position <- 50
  for (i in seq_along(input$turn)) {
    zeros <- 0
    turns_mod <- input$dist[i] %% 100
    # times passed 0 (hundreds)
    if (any_pass) {
      zeros <- (input$dist[i] - turns_mod) / 100
    }
    
    if (input$dir[i] == "L") {     # left
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
        "Position after turn ", input$turn[i], ": ", position, 
        " (", zeros, " zeros)"
      )
    }
  }
  message("The password is ", counter)
}

# part 1
d1(example=T, any_pass=F)
d1(example=F, any_pass=F)
# part 2
d1(example=T, any_pass=T)
d1(example=F, any_pass=T)

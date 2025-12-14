# https://adventofcode.com/2025/day/10

#### part 1 ####
# What is the fewest button presses required to correctly configure
# the indicator lights on all of the machines?
d10p1 <- function(day = 10, year = 2025, example = TRUE) {
  verbose <- example
  input <- read_split(day, year, example, " ")
  n_machines <- length(input)
  # machine_seq <- 1:length(input)
  
  # sequence of lights to turn on the machine
  lights_correct <- input |> 
    lapply(`[[`, 1) |>                           # first element are lights
    lapply(\(x) substr(x, 2, nchar(x) - 1) ) |>  # remove brackets
    lapply(\(x) strsplit(x, "") |> unlist() ) |> # split, only one element
    lapply(\(x) x == "#" )                       # boolify
  # parse buttons
  buttons <- input |>
    lapply(\(x) x[-c(1, length(x))] ) |>   # all elements except first and last
    lapply(\(x) substr(x, 2, nchar(x) - 1) ) |>  # remove parentheses
    lapply(\(x) strsplit(x, ",") ) |>            # csv
    lapply(\(x) lapply(x, \(y) as.numeric(y) + 1 ) ) # convert to numeric
  
  # initally all lights are off
  lights_off <- lapply(lights_correct, \(x) rep(FALSE, length(x)))
  correct_n_presses <- rep(NA, n_machines)
  
  # m <- 1
  # b <- 1
  # comb <- 2
  lights_tmp <- lights_off
  n <- 1
  while (any(is.na(correct_n_presses))) {
    message("Trying out ", n, " presses for ", 
            sum(is.na(correct_n_presses)), " machines")

    # iterate over machines which are not yet solved
    na_machines <- which(is.na(correct_n_presses))
    
    # button indices of unique combinations of n presses
    # list of arrays
    # presses in cols, buttons in rows
    button_lengths <- sapply(buttons[na_machines], length)
    button_comb_ind <- 
      unique(button_lengths) |> 
      lapply(\(x) seq_len(x) |> rep(n) |> combn(n) )
    # names(button_comb_ind) <- unique(button_len)
    
    for (m in na_machines) {
      which_combination <- which(buttons[[m]] |> length() == unique(button_lengths))
       
      # iterate over combinations of machine
      machine_combs <- ncol(button_comb_ind[[which_combination]])
      if (length(na_machines) < 30) { 
        message("Machine ", m, " has ", machine_combs, " possible combinations.")}
      for (comb in seq_len(machine_combs) ) { # col
        
        # iterate over buttons of combination
        for (b in button_comb_ind[[which_combination]][,comb] ) {   # row
          lights_tmp[[m]][buttons[[m]][[b]]] <- !lights_tmp[[m]][buttons[[m]][[b]]]
        }
        # check if state is correct
        if (all(lights_tmp[[m]] == lights_correct[[m]])) { 
          correct_n_presses[m] <- n
          next # skip remaining botton presses
        }
         # reset for next combination
        lights_tmp[[m]] <- lights_off[[m]]
        
      }
    }
    
    n <- n + 1 # try out one button press more
  }
  
  return(sum(correct_n_presses))
}

d10p1(example = TRUE) == 7
d10p1(example = FALSE)

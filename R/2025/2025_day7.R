#### https://adventofcode.com/2025/day/7 ####

d7 <- function(day = 7, year = 2025, example = TRUE, part = 1) {
  verbose <- example
  input <- 
    read_split(day, year, example) |> 
    do.call(what = rbind)
  # remove lines without splitters
  input <- input[seq(1, ncol(input), 2), ]
  
  # iterate over rows
  n_cols <- ncol(input)
  n_rows <- nrow(input)
  beam_prev <- input[1, ] == "S"
  split_counter <- 0L
  beams <- beam_prev
  for (i in 2:n_rows) {
    # find splitting positions and count
    split_here <- (input[i, ] == "^") & beam_prev
    split_ind <- which(split_here)
    split_counter <- split_counter + length(split_ind)
    
    # get new beam locations
    beam_next <- beam_prev & !split_here
    beam_next[split_ind - 1] <- TRUE
    beam_next[split_ind + 1] <- TRUE
    
    # save for next iteration
    beams <- rbind(beams, beam_next)
    beam_prev <- beam_next
  }
  
  if (verbose) { # print tree
    p <- input
    p[beams]  <- "|"
    p |> 
      cbind("\n") |>
      apply(1, paste0, collapse = " ") |> 
      cat()
  }
  
  # How many times will the beam be split?
  if (part == 1) { return(split_counter) }
  
  # How many different paths are there to the bottom?
  if (part == 2) {
    # make adjacency list
    # name: node
    # value: children of node
    adj <- list()
    for (r in seq_len(n_rows - 1)) { # omit last row
      for (c in seq_len(n_cols)) {
        
        if ( beams[r, c] ) { # is beam
          node <- paste0(r, ",", c)
          next_row <- r + 1
          if ( beams[next_row, c] ) { # not split
            adj[[node]] <- paste0(next_row, ",", c)
          } else {                    # split
            # add check for cols out of bounds to generalize
            adj[[node]] <- paste0(next_row, ",", c + c(-1, 1))
          }
        }
        
      }
    }
    
    # count paths recursively
    start_node <- paste0("1,", which(beams[1, ])) # single start node
    end_nodes <- paste0(n_rows, ",", which(beams[n_rows, ])) # multiple end nodes
    memo_env <- new.env(parent = emptyenv())
    n_paths <- count_paths(
      node = start_node, 
      end_nodes = end_nodes, 
      adj = adj, 
      memo = memo_env
    )
    
    return(n_paths)
  }
}

#### count paths ####
count_paths <- function(node, end_nodes, adj, memo) {
  
  # 1. Memo lookup
  if (exists(node, memo, inherits = FALSE)) {
    return(memo[[node]])
  }
  
  # 2. Base case: node is an end node
  if (node %in% end_nodes) {
    memo[[node]] <- 1
    return(1)
  }
  
  # 3. Recursive case
  children <- adj[[node]]
  
  # If no children, this is a dead end
  if (length(children) == 0) {
    memo[[node]] <- 0
    return(0)
  }
  
  total <- 0
  for (child in children) {
    total <- total + 
      count_paths(
        node = child, # new start node is child -> recursion
        end_nodes = end_nodes, 
        adj = adj, 
        memo = memo
      )
  }
  
  # 4. Store and return
  memo[[node]] <- total
  return(total)
}

#### run ####
# part 1
d7(example = TRUE, part = 1) == 21
d7(example = FALSE, part = 1) == 1546

# part 2
d7(example = TRUE, part = 2) == 40
d7(example = FALSE, part = 2) == 13883459503480

# benchmark
microbenchmark::microbenchmark(
  d7(example = FALSE, part = 1), #   2.4 ms
  d7(example = FALSE, part = 2), # 288.3 ms
  times = 10
)

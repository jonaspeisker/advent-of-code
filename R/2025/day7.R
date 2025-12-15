# https://adventofcode.com/2025/day/7

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
  for (i in 2:nrow(input)) {
    # find splitting positions and count
    # i <- 5
    split_here <- (input[i, ] == "^") & beam_prev
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
    p <- input
    p[beams]  <- "|"
    p <- p |> 
      cbind("\n") |>
      apply(1, paste0, collapse = " ") |> 
      cat()
    
    cat(p)
  }
  
  # How many times will the beam be split?
  if (part == 1) { return(split_counter) }
  
  if (part == 2) {
  # adjecency list
  adj <- list()
  for (r in seq_len(n_rows)) {
    for (c in seq_len(n_cols)) {

      if (!beams[r, c]) next
      
      node <- paste0(r, ",", c)
      children <- character(0)
      next_row <- r + 1
      next_col <- c
      if (next_row <= n_rows && next_col >= 1 && next_col <= n_cols) {
        if (beams[next_row, next_col]) {
          child <- paste0(next_row, ",", next_col)
          adj[[node]] <- child
          next
        }
      } #else {
      #   child <- paste0(next_row, ",", next_col + c(-1, 1))
      #   adj[[node]] <- child
      # }
      
      # only add if no path down
      for (dc in c(-1, 1)) {
        next_col <- c + dc
        if (next_row <= n_rows && next_col >= 1 && next_col <= n_cols) {
          if (beams[next_row, next_col]) {
            child <- paste0(next_row, ",", next_col)
            children <- c(children, child)
          }
        }
      }
      adj[[node]] <- children
    }
  }

  # adj
  start_node <- paste0("1,", which(beams[1, ])) # single start node
  end_nodes <- paste0(n_rows, ",", which(beams[n_rows, ]))
  memo_env <- new.env(parent = emptyenv())
  n_paths <- count_paths(node = start_node, end_nodes = end_nodes, adj = adj, memo = memo_env)
  # memo_env |> View()
  return(n_paths)
  }
}

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
      count_paths(node = child, end_nodes = end_nodes, adj = adj, memo = memo)
  }
  
  # 4. Store and return
  memo[[node]] <- total
  return(total)
}

# part 1
d7(example = TRUE, part = 1) == 21
d7(example = FALSE, part = 1) == 1546
# part 2
d7(example = TRUE, part = 2) == 40
d7(example = FALSE, part = 2) == 13883459503480
# benchmark
microbenchmark(
  d7(example = FALSE, part = 1), #   2.8 ms
  d7(example = FALSE, part = 2), # 311.5 ms
  times = 10
)

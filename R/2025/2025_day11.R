# https://adventofcode.com/2025/day/11

d11 <- function(day = 11, year = 2025, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example, example_part = part, split = ": ")
  
  # make adjacency list
  names(input) <- input |> sapply(`[`, 1) # set first element to name (node)
  # remove first element and split (children)
  adj_list <- input |> lapply(\(x){ strsplit(x[-1], " ")[[1]] }) 
  
  if (part == 1) { # How many different paths lead from you to out?
    nodes <- c("you", "out")
  }
  if (part == 2) { # How many paths from svr to out visit both dac and fft?
    nodes <- c("svr", "fft", "dac", "out")
  }
  
  n_paths <- c()
  for (i in seq_len(length(nodes) - 1)) {
    # count paths recursively
    start_node <- nodes[i] # single start node
    end_nodes <- nodes[i + 1] # multiple end nodes
    memo_env <- new.env(parent = emptyenv())
    n_paths_tmp <- dfs_count_paths( # R/utils/dfs_search.R
      node = start_node, 
      end_nodes = end_nodes, 
      adj = adj_list, 
      memo = memo_env
    )
    n_paths <- c(n_paths, n_paths_tmp)
    
  }
  return(Reduce(`*`, n_paths))
}

# part 1
d11(example = TRUE,  part = 1) == 5
d11(example = FALSE, part = 1) == 764

# part 2
d11(example = TRUE,  part = 2) == 2
d11(example = FALSE, part = 2) == 462444153119850

# benchmark
microbenchmark::microbenchmark(
  d11(example = FALSE, part = 1), #  5 ms
  d11(example = FALSE, part = 2)  # 20 ms
)


# https://adventofcode.com/2025/day/12

d12 <- function(day = 12, year = 2025, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)
  is_sep <- input == ""
  groups <- cumsum(is_sep)
  input_sep <- input[!is_sep] |> split(groups[!is_sep])
  input_len <- length(input_sep)
  
  # shape of the presents
  patterns <- 
    input_sep[-input_len] |> # omit last element
    lapply(\(x) x[-1] |> strsplit("") |> do.call(what = rbind)) |> 
    lapply(\(x) x == "#") # boolean array
  # contains size of area under tree and required shapes
  trees <- 
    input_sep[input_len] |> 
    unlist() |> 
    strsplit(": ")
  names(trees) <- NULL
  regions <- 
    sapply(trees, `[`, 1) |> 
    strsplit("x") |> 
    lapply(as.integer) # list of vectors
  presents <- 
    sapply(trees, `[`, -1) |> 
    strsplit(" ") |> 
    lapply(as.integer) # list of vectors

  if (part == 1) {
  
    return()
  }
  
  if (part == 2) {
  
    return()
  }
}

# part 1
d12(example = TRUE,  part = 1) ==
d12(example = FALSE, part = 1)

# part 2
d12(example = TRUE,  part = 2) ==
d12(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d12(example = FALSE, part = 1),
  d12(example = FALSE, part = 2)
)


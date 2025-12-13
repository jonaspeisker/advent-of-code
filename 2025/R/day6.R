#### part 1 ####
d6p1 <- function(day = 6, example = TRUE) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.table() |> 
    as.matrix()
  
  # reduce cols with respective operator
  nr <- nrow(input)
  reduced <- sapply(seq_len(ncol(input)), function(i) {
    op <- match.fun(input[nr, i])             # last row contains operator
    Reduce(op, as.numeric(input[1:(nr-1),i])) # others rows contain numbers
  })
  return(sum(reduced))
}

d6p1(example = TRUE) == 4277556
d6p1(example = FALSE) == 4693159084994

#### part 2 ####
d6p2 <- function(day = 6, example = TRUE) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() |> 
    strsplit("") # list of char vectors
  n_row <- length(input)
  # matrix of digits and spaces
  nums <- 
    input[1:(n_row-1)] |> 
    do.call(what = rbind)
  # vector of operators in last row
  op_raw <- input[[n_row]]
  op <- op_raw[op_raw != " "]
  # paste together columns
  ceph_raw <- apply(nums, 2, function(col) {
    paste0(col, collapse = "")
    })
  # find separators (all rows are empty)
  spaces <- rep(" ", nrow(nums)) |> paste0(collapse="")
  is_sep <- (ceph_raw == spaces)
  # split into list of vectors
  groups <- cumsum(is_sep)
  ceph <- split(ceph_raw[!is_sep], groups[!is_sep])
  # reduce cols with respective operator
  reduced <- sapply(1:length(ceph), function(i) {
    op <- match.fun(op[i])
    Reduce(op, as.numeric(ceph[[i]]))
  })
  return(sum(reduced))
}

d6p2(example = TRUE) == 3263827
d6p2(example = FALSE) == 11643736116335

# benchmark
microbenchmark(
  d6p1(example = FALSE), # 38.1 ms
  d6p2(example = FALSE)  # 24.0 ms
)

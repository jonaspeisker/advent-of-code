#### part 1 ####
d6p1 <- function(day=6, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.table()
  
  # reduce cols with respective operator
  nr <- nrow(input)
  reduced <- sapply(seq_len(ncol(input)), function(i) {
    op <- match.fun(input[nr, i])             # last row contains operator
    Reduce(op, as.numeric(input[1:(nr-1),i])) # others rows contain numbers
  })
  # return
  message("The grand total is ", sum(reduced))
}

d6p1(example=T)
d6p1(example=F)

#### part 2 ####
d6p2 <- function(day=6, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() |> 
    strsplit("") # list of char vectors
  
  # matrix of digits and spaces
  nums <- do.call(rbind, input[1:(length(input)-1)])
  # vector of operators
  op_raw <- input[[length(input)]]
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
  # return
  message("The grand total is ", sum(reduced))
}

d6p2(example=T)
d6p2(example=F)

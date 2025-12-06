# part 1
grand_total_p1 <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day6/",
    verbose=FALSE
) {
  input <- read.table(paste0(path, file_name))
  # reduce cols with respective operator
  nr <- nrow(input)
  reduced <- sapply(seq_len(ncol(input)), function(i) {
    op <- match.fun(input[nr, i])             # last row contains operator
    Reduce(op, as.numeric(input[1:(nr-1),i])) # others rows contain numbers
  })

  grand_total <- sum(reduced)
  message("The grand total is ", grand_total)
}

grand_total_p1("input_example.txt", verbose=T)
grand_total_p1("input.txt")

# part 2
grand_total_p2 <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day6/",
    verbose=FALSE
) {
  input <- # list of char vectors
    readLines(paste0(path, file_name)) |> 
    strsplit("")
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
  
  grand_total <- sum(reduced)
  message("The grand total is ", grand_total)
}

grand_total_p2("input_example.txt", verbose=T)
grand_total_p2("input.txt")

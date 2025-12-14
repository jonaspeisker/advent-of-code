# https://adventofcode.com/2025/day/4

d4 <- function(day = 4, year = 2025, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example)
  bin <- # convert to binary matrix
    lapply(input, function(x) { as.integer(x == "@") }) |> 
    do.call(what = rbind)
  
  # How many rolls are there with < 4 rolls of paper in the adjacent positions?
  if (part == 1) {
    nb_sum <- neighbor_sum(bin)
    return(sum(bin == 1 & nb_sum < 4))
  }
  
  # How many rolls of paper in total can be removed?
  if (part == 2) {
    acc_rolls <- 1L
    bin_tmp <- bin
    while (acc_rolls > 0) {
      nb_sum <- neighbor_sum(bin_tmp)             # matrix with number of nb
      acc_roll_mat <- (bin_tmp == 1 & nb_sum < 4) # rolls with <4 neighbors
      acc_rolls <- sum(acc_roll_mat)              # count accessible rolls 
      bin_tmp[acc_roll_mat] <- 0                  # remove accessible rolls
    }
    return(sum(bin) - sum(bin_tmp))
  } 
}

#### make matrix with sum of neighboring cells ####
neighbor_sum <- function(mat) {
  m <- rbind(0, mat, 0); m <- cbind(0, m, 0) # pad with zeros
  nr <- nrow(m); nc <- ncol(m)
  out <-                                     # sum shifted mats  
    m[-c(nr,nr-1), -c(nc,nc-1)] + m[-c(nr,nr-1), -c(1,nc)] + m[-c(nr,nr-1), -c(1,2)] +
    m[-c(1,nr),    -c(nc,nc-1)] +                          + m[-c(1,nr),    -c(1,2)] +
    m[-c(1,2),     -c(nc,nc-1)] + m[-c(1,2),     -c(1,nc)] + m[-c(1,2),     -c(1,2)]
  return(out)
}

#### run ####
#part 1
d4(example = TRUE, part = 1) == 13
d4(example = FALSE, part = 1) == 1516
#part 2
d4(example = TRUE, part = 2) == 43
d4(example = FALSE, part = 2) == 9122
# benchmark
microbenchmark(
  d4(example = FALSE, part = 1), #  2.1 ms
  d4(example = FALSE, part = 2)   # 70.5 ms
)

accessible_rolls <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day4/",
    recursive=FALSE
    ) {
  input <- readLines(paste0(path, file_name))
  split <- input |> strsplit("")
  bin <-
    sapply(split, function(x){
      ifelse(x == "@", 1, 0)
    }) |> 
    t()
  if (recursive) {
    acc_rolls <- 1
    bin_tmp <- bin
    while (acc_rolls > 0) {
      nb_sum <- neighbor_sum(bin_tmp)             # matrix with number of nb
      acc_roll_mat <- (bin_tmp == 1 & nb_sum < 4) # rolls with <4 neighbors
      acc_rolls <- sum(acc_roll_mat)              # count accessible rolls 
      bin_tmp[acc_roll_mat] <- 0                  # remove accessible rolls
    }
    return(sum(bin) - sum(bin_tmp))
  } else{
    nb_sum <- neighbor_sum(bin)
    return(sum(bin == 1 & nb_sum < 4))
  }
}

neighbor_sum <- function(mat) {
  m <- rbind(0, mat, 0)   # pad with zeros
  m <- cbind(0, m, 0)
  nr <- nrow(m)
  nc <- ncol(m)
  out <-                  # sum shifted mats  
    m[-c(nr,nr-1), -c(nc,nc-1)] + m[-c(nr,nr-1), -c(1,nc)] + m[-c(nr,nr-1), -c(1,2)] +
    m[-c(1,nr),    -c(nc,nc-1)] +                          + m[-c(1,nr),    -c(1,2)] +
    m[-c(1,2),     -c(nc,nc-1)] + m[-c(1,2),     -c(1,nc)] + m[-c(1,2),     -c(1,2)]
  out
}

#part 1
accessible_rolls()
accessible_rolls("input.txt")
#part 1
accessible_rolls(recursive=TRUE)
accessible_rolls("input.txt", recursive=TRUE)

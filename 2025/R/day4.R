#### main ####
d4 <- function(day=4, example=T, recursive=F) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() |>
    strsplit("")
  
  bin <- sapply(input, function(x){
    ifelse(x == "@", 1, 0) }) |> 
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
d4(example=T)
d4(example=F)
#part 2
d4(example=T, recursive=T)
d4(example=F, recursive=T)

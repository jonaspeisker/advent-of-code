# https://adventofcode.com/2015/day/7

d7 <- function(day = 7, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_split(day, year, example, " ")
  
  instr <- lapply(input, \(x) x[1:(length(x)-2)] )
  names(instr) <- sapply(input, \(x) x[length(x)])
  
  and <- sapply(input, \(x) x[2] == "AND")
  or <- sapply(input, \(x) x[2] == "OR")
  lshift <- sapply(input, \(x) x[2] == "LSHIFT")
  rshift <- sapply(input, \(x) x[2] == "RSHIFT")
  not <- sapply(input, \(x) x[1] == "NOT")
  
  if (part == 1) {
    # result, named vector
    res <- rep(NA, length(instr))
    names(res) <- names(instr)
    
    # initial values
    is_l1 <- sapply(instr, length) == 1
    res[which(is_l1)] <- sapply(instr[is_l1], as.integer)
    
    while (any(is.na(res))) { #is.na(res["a"])
      available <- !is.na(res)  
      instr_left <- instr[!available]
      
      # i <- 1
      for (i in seq_len(length(instr_left))) {
        instr_len <- length(instr_left[[i]])
                            
        if (instr_len == 3) { 
          x <- instr_left[[i]]
          int1 <- as.integer(x[1])
          int2 <- as.integer(x[3])
          n1 <- ifelse(is.na(int1), res[x[1]], int1)
          n2 <- ifelse(is.na(int2), res[x[3]], int2)
          if (any(is.na(c(n1, n2)))) { next } else { 
          if (and[!available][i]) { 
            res[names(instr_left[i])] <- bitwAnd(n1, n2)
          } else if (or[!available][i]) { 
            res[names(instr_left[i])] <- bitwOr(n1, n2)
          } else if (lshift[!available][i]) { 
            res[names(instr_left[i])] <- bitwShiftL(n1, n2)
          } else if (rshift[!available][i]) { 
            res[names(instr_left[i])] <- bitwShiftR(n1, n2)
          }
          }
        } else if (instr_len == 2) { 
          x <- instr_left[[i]]
          int1 <- as.integer(x[2])
          n1 <- ifelse(is.na(int1), res[x[2]], int1)
          if (is.na(n1)) { next } else { 
            res[names(instr_left[i])] <- bitwNot(n1)
          }
        }
      }
    }
    # print(instr_left)
    return(res)
  }
  
  if (part == 2) {
    
    return()
  }
}

# part 1
d7(example = TRUE,  part = 1) 
d7(example = FALSE, part = 1)

# part 2
d7(example = TRUE,  part = 2) ==
  d7(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d7(example = FALSE, part = 1),
  d7(example = FALSE, part = 2)
)


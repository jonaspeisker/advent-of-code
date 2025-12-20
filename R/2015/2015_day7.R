#### https://adventofcode.com/2015/day/7 ####

d7 <- function(day = 7, year = 2015, example = TRUE, part = 1) {
  # parse input
  input <- read_split(day, year, example, " ") # list of char vectors
  # omit last two elements to get instruction 
  instr <- input |> lapply(\(x) x[1:(length(x) - 2)])
  # set names to right hand side of expression (last element)
  names(instr) <- input |> sapply(\(x) x[length(x)])
  # check which operation to apply
  and    <- input |> sapply(\(x) x[2] == "AND")
  or     <- input |> sapply(\(x) x[2] == "OR")
  lshift <- input |> sapply(\(x) x[2] == "LSHIFT")
  rshift <- input |> sapply(\(x) x[2] == "RSHIFT")
  not    <- input |> sapply(\(x) x[1] == "NOT")
  
  # what signal is ultimately provided to wire a?
  return_wire <- ifelse(example, "i", "a")
  if (part == 2) {
    # take the signal you got on wire a, override wire b to that signal
    instr[["b"]] <- as.character(46065)
  }
  
  # create named vector to hold results
  res <- rep(NA_integer_, length(instr))
  names(res) <- names(instr)
  # initial literal values
  is_lit <- sapply(instr, length) == 1
  # could be NA
  res[is_lit] <- instr[is_lit] |> unlist() |> as.integer()

  res_is_na <- is.na(res)
  old_na <- Inf
  while (any(res_is_na)) {
    if (sum(res_is_na) == old_na) {
      stop("No further progress possible: unresolved dependencies")
    }
    
    # iterate over instructions without result
    instr_left <- instr[res_is_na]
    for (i in seq_along(instr_left)) {
      
      x <- instr_left[[i]]
      x_name <- names(instr_left)[i]
      x_len <- length(x)
      
      if (x_len == 3) {                    # AND, OR, SHIFTL, SHIFTR
        
        n1 <- if (is_number(x[1])) as.integer(x[1]) else res[x[1]]
        n2 <- if (is_number(x[3])) as.integer(x[3]) else res[x[3]]
        if (any(is.na(c(n1, n2)))) next
        
        if (and[res_is_na][i]) {           # AND
          res[x_name] <- mask16(bitwAnd(n1, n2))
          
        } else if (or[res_is_na][i]) {     # OR
          res[x_name] <- mask16(bitwOr(n1, n2))
          
        } else if (lshift[res_is_na][i]) { # SHIFTL
          res[x_name] <- mask16(bitwShiftL(n1, n2))
          
        } else if (rshift[res_is_na][i]) { # SHIFTR
          res[x_name] <- logical_rshift(n1, n2)
        }
        
      } else if (x_len == 2) {             # NOT
        
        n1 <- if (is_number(x[2])) as.integer(x[2]) else res[x[2]]
        if (is.na(n1)) next
        res[x_name] <- mask16(bitwNot(n1)) # NOT
        
      } else if (x_len == 1) {             # reference to other wire
        
        res[x_name] <- res[x[1]] # could be NA
        
      } else {
        stop("Instruction of unexpected length.")
      }
    }
    
    old_na <- sum(res_is_na) # save old number of NAs
    res_is_na <- is.na(res)  # update NAs
  }
  
  return(res[[return_wire]])
}

#### helpers ####
# R integer are signed 32 bit
# convert to unsigned 16 bit
# 0xFFFF is largest 16 bit in hexadecimal notation (all 1)
# Numbers starting with 0x are hexadecimal
# Decimal:      0 1 2 ... 9 10 11 12 13 14 15
# Hexadecimal:  0 1 2 ... 9  A  B  C  D  E  F
# 0xFFFF = 00000000 00000000 11111111 11111111
# x      = 00000000 00000001 00000010 00110100
# AND    = 00000000 00000000 00000010 00110100
# sets all the highest 16 bits to 0
mask16 <- function(x) bitwAnd(x, 0xFFFF)

# left shift does not depend on signedness
# but right shift does
# bitwShiftR() expects signed int
# masking always sets the sign bit to 0
logical_rshift <- function(x, n) {
  bitwShiftR(mask16(x), n)
}

# check if string is number
is_number <- function(x) {
  grepl("^[0-9]+$", x)
  }

#### run ####
# part 1
d7(example = TRUE,  part = 1) == 65079
d7(example = FALSE, part = 1) == 46065

# part 2
d7(example = FALSE, part = 2) == 14134

# benchmark
microbenchmark::microbenchmark(
  d7(example = FALSE, part = 1), # 240 ms
  d7(example = FALSE, part = 2), # 243 ms
  times = 10
)


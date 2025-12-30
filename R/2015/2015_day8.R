# https://adventofcode.com/2015/day/8

d8 <- function(day = 8, year = 2015, example = FALSE, part = 1) {
  input <- read_input(day, year, example)
  # number of chars in code representation of the string literal
  nchar_literal <- nchar(input)
  
  # what is the number of characters of code for string literals 
  # minus the number of characters in memory for the values of the strings?
  if (part == 1) {
    n_esc <- 
      input |> # match \\\"
      gregexpr(pattern = "\\\\[\\\\\"]") |> 
      regmatches(x = input) |> 
      lengths() # short for sapply(x, length)
    n_hex <- 
      input |> # match \x01
      gregexpr(pattern = "\\\\x[0-9A-Fa-f]{2}") |> 
      regmatches(x = input) |> 
      lengths() 
    
    # 2: enclosing quotes
    # n_esc: number of escaped characters
    # n_hex: number of hex ASCII codes
    nchar_mem <- nchar_literal - 2 - n_esc - 3*n_hex
    return(sum(nchar_literal) - sum(nchar_mem))
  }
  
  # the total number of characters to represent the newly encoded strings 
  # minus the number of characters of code in each original string literal
  if (part == 2) {
    n_quote <- 
      input |> # match "
      gregexpr(pattern = "\"") |> 
      regmatches(x = input) |> 
      lengths()
    n_slash <- 
      input |> # match \\
      gregexpr(pattern = "\\\\") |> 
      regmatches(x = input) |> 
      lengths()

    nchar_mem <- nchar_literal + n_quote + n_slash + 2    
    return(sum(nchar_mem) - sum(nchar_literal))
  }
}

# part 1
d8(example = TRUE,  part = 1) == 12
d8(example = FALSE, part = 1) == 1333

# part 2
d8(example = TRUE,  part = 2) == 19
d8(example = FALSE, part = 2) == 2046

# benchmark
microbenchmark::microbenchmark(
  d8(example = FALSE, part = 1), # 3.7 ms
  d8(example = FALSE, part = 2)  # 4.5 ms
)

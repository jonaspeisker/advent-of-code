# https://adventofcode.com/2015/day/8

d8 <- function(day = 8, year = 2015, example = FALSE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)
  nchar_literal <- nchar(input)
  
  if (part == 1) {
    hex_regex <- "\\\\x[0-9A-Fa-f]{2}"
    input_sub <- 
      input |>
      gsub(
        pattern = "\\\"",
        replacement = "\'",
        fixed = TRUE
      ) |> 
      gsub(
        pattern = "\\\\",
        replacement = "\\",
        fixed = TRUE
      )

    hex_subtr <- 
      regmatches(input, gregexpr("\\\\x[0-9A-Fa-f]{2}", input)) |> 
      sapply(\(x) length(x) * 3 )

    nchar_mem <- nchar(input_sub) - 2 - hex_subtr
    
    return(sum(nchar_literal) - sum(nchar_mem))
  }
  
  if (part == 2) {
    input_sub <- 
      input |>
      gsub(
        pattern = "\\",       # 1 escaped backslash
        replacement = "\\\\", # 2 escaped backslashes
        fixed = TRUE
      ) |>
      gsub(
        pattern = "\\\"",
        replacement = "\\\\'",
        fixed = TRUE
      )
    
  nchar_mem <- nchar(input_sub) + 4 
    
  return(sum(nchar_mem) - sum(nchar_literal))
  }
}

cat(input)
cat(input_sub)

# part 1
d8(example = TRUE,  part = 1) == 12
d8(example = FALSE, part = 1) == 1333

# part 2
d8(example = TRUE,  part = 2) == 19
d8(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  d8(example = FALSE, part = 1),
  d8(example = FALSE, part = 2)
)



hex_to_char <- function(x) {
  regmatches(x, gregexpr("\\\\x[0-9A-Fa-f]{2}", x)) |> 
    lapply(function(x) {
      x |> 
        substr(3, 5) |> 
        strtoi(16) |> 
        as.raw() |> 
        rawToChar()
    })
}
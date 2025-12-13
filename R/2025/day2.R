# https://adventofcode.com/2025/day/2

d2 <- function(
    day = 2, year = 2025, example = TRUE, 
    part = 1, method = "regex"
  ) {
  stopifnot(
    part %in% 1:2,
    method %in% c("arithmetic", "string", "regex")
  )
  verbose <- example
  input <- 
    get_file_name(day, year, example) |>
    readLines() |>                 # single line
    strsplit(",") |>               # csv
    unlist() |>                    # unlist list of length 1
    strsplit("-") |>               # list of vectors
    do.call(what = rbind)          # bind to array
  storage.mode(input) <- "numeric" # type convert
  
  # iterate over ranges
  sum_invalid <- 0
  for (i in seq_len(nrow(input))) {
    # faster to expand each range separately in for loop than expanding all
    # at once due to more efficient memory allocation
    input_seq <- input[i, 1]:input[i, 2]
    
    # invalid if number is ONLY a sequence of digits repeated EXACTLY twice
    if (part == 1) {
      if (method == "arithmetic"){
        d <- floor(log10(input_seq)) + 1
        half <- d / 2
        pow <- 10^half
        high <- input_seq %/% pow
        low  <- input_seq %% pow
        invalid <- (high == low)
      }
      
      if (method == "string") {
        s <- as.character(input_seq)
        n <- nchar(s)
        half <- n / 2
        invalid <- substr(s, 1, half) == substr(s, half + 1, n)
      }
      
      if (method == "regex"){
        invalid <- grepl("^(.+)\\1$", input_seq)
      }
    }
    
    # invalid if number is ONLY a sequence of digits repeated AT LEAST twice
    # check single value against sequence of given length
    if (part == 2) {
      invalid <- grepl("^(.+)\\1+$", input_seq)
    }
    
    if (verbose) { cat("Invalid IDs: ", input_seq[invalid], "\n") }
    sum_invalid <- sum_invalid + sum(input_seq[invalid])
  }
  
  return(sum_invalid)
}

# part 1
d2(example = TRUE, part = 1) == 1227775554
d2(example = FALSE, part = 1) == 31839939622
# part 2
d2(example = TRUE, part = 2) == 4174379265
d2(example = FALSE, part = 2) == 41662374059
# benchmark
microbenchmark(
  d2(example = FALSE, part = 1, method = "arithmetic"), # 0.7 sec
  d2(example = FALSE, part = 1, method = "string"),     # 1.6 sec
  d2(example = FALSE, part = 1, method = "regex"),      # 2.0 sec
  d2(example = FALSE, part = 2),                        # 2.0 sec
  times = 10
)

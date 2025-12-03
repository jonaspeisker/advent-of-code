invalid_id_sum <- function(file_name, pattern) {
  input_raw <- readLines(file_name, warn = F) 
  # parse input
  input_sep <- strsplit(input_raw, ",")[[1]]
  input_sep2 <- strsplit(input_sep, "-") 
  # expand sequence based on start and stop value
  input <- 
    lapply(input_sep2, function(x) {x[1]:x[2]}) |> # list of vectors
    unlist()                                       # vector
  # check for pattern
  invalid <- 
    lapply(input, ifelse(pattern == "part1", check_part1, check_part2)) |> 
    unlist()
  # sum invalid values
  return(input[invalid] |> sum())
}

#### part 1 ####
# check if number is ONLY a sequence of digits repeated EXACTLY twice
check_part1 <- function(values) { # vector
  digits <- nchar(values)
  ifelse(
    digits %% 2 != 0 | digits == 1, # if has odd number of digits or single digit
    FALSE,                          # always FALSE
    # else compare first half with second half
    substr(values, 1, digits/2) == substr(values, digits/2+1, digits)
  )
}
check_part1(c(2, 1212, 9.394e+09))

#### part 2 ####
# check if number is ONLY a sequence of digits repeated AT LEAST twice
# check single value against sequence of given length
# sl: sequence length, v: value, d: digits
test_seq <- function(sl, v, d) { 
  test <- 
    Reduce(
      paste0, 
      rep(substr(v, 1, sl), times=d/sl)
    )
  return(v == test)
}

check_part2 <- function(values) { # vector
  sapply(values, function(value){
    digits <- nchar(value)
    if (digits == 1) {return(FALSE)}
    # seq has to occur at least 2x
    max_seq_length <- floor(digits/2)
    invalid <- Reduce(
      any, 
      # iterate over seq length
      lapply(1:max_seq_length, test_seq, v=value, d=digits)
    )
    return(invalid)
  })
}
check_part2(c(2, 1212, 9.394e+09))

#### run ####
# part 1
invalid_id_sum("day2/input_example.txt", pattern="part1")
invalid_id_sum("day2/input.txt", pattern="part1")
# part 2
invalid_id_sum("day2/input_example.txt", pattern="part2")
invalid_id_sum("day2/input.txt", pattern="part2")
# compare time
invalid_id_sum("day2/input_example.txt", pattern="part1") |> system.time()
invalid_id_sum("day2/input_example.txt", pattern="part2") |> system.time()

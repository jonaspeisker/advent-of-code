invalid_id_sum <- function(file_name, pattern) {
  input_raw <- readLines(file_name, warn = F) 
  # parse input
  input_sep <- strsplit(input_raw, ",")[[1]]
  input_sep2 <- strsplit(input_sep, "-")
  # expand values and check
  input <- 
    lapply(input_sep2, function(x) {x[1]:x[2]}) |> 
    unlist()
  invalid <- 
    lapply(input, ifelse(pattern == "part1", check_part1, check_part2)) |> 
    unlist()
  # return sum
  return(
    input[invalid] |> sum()
    )
}

#### part 1 ####
# check if number is ONLY a sequence of digits repeated EXACTLY twice
check_part1 <- function(values) {
  digits <- nchar(values)
  ifelse(
    digits %% 2 == 0, # has even number of digits
    substr(value, 1, digits/2) == substr(value, digits/2+1, digits),
    FALSE # odd number always FALSE
  )
}

#### part 2 ####
# check if number is ONLY a sequence of digits repeated AT LEAST twice
check_part2 <- function(values) {
  sapply(values, function(value){
    digits <- nchar(value)
    if (digits == 1) {return(FALSE)}
    # seq has to occur at least 2x
    max_seq_length <- floor(digits/2)
    invalid <- Reduce(
      any, 
      lapply(1:max_seq_length, test_seq, v=value, d=digits)
    )
    return(invalid)
  })
}
check_part2(9.394e+09)

test_seq <- function(sl, v, d) {
  test <- 
    Reduce(
      paste0, 
      rep(substr(v, 1, sl), times = d/sl)
    )
  return(v == test)
}

#### run ####
invalid_id_sum("day2/input_example.txt", pattern = "part1")
invalid_id_sum("day2/input.txt", pattern = "part1")

invalid_id_sum("day2/input_example.txt", pattern = "part2")
invalid_id_sum("day2/input.txt", pattern = "part2")


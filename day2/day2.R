# you can find the invalid IDs by looking for any ID which is made only of 
# some sequence of digits repeated twice. So, 55 (5 twice), 6464 (64 twice), 
# and 123123 (123 twice) would all be invalid IDs.

check_repeat_p1 <- function(value) {
  digits <- nchar(value)
  ifelse(
    digits %% 2 == 0, # has even number of digits
    substr(value, 1, digits/2) == substr(value, digits/2+1, digits),
    FALSE # odd number always FALSE
  )
}
check_repeat_p1(c(1212, 1111))

file_name <- "day2/input_example.txt"
invalid_id_sum <- function(file_name, pattern) {
  input_raw <- readLines(file_name, warn = F) 
  # parse input
  input_sep <- strsplit(input_raw, ",")[[1]]
  input_sep2 <- strsplit(input_sep, "-")
  # expand values and check
  input <- 
    lapply(input_sep2, function(x) {x[1]:x[2]}) |> 
    unlist()
  if (pattern == "part1") {
    invalid <- 
      lapply(input, check_repeat_p1) |> 
      unlist()
  } else if (pattern == "part2") {
    invalid <- 
      lapply(input, check_repeat_p1) |> 
      unlist()
  } else {
    stop("Pattern should be part1 or part2.")
  }
  # return sum
  return(
    input[invalid] |> sum()
    )
}

invalid_id_sum("day2/input_example.txt", pattern = "part1")
invalid_id_sum("day2/input.txt", pattern = "part1")

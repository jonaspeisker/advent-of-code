# https://adventofcode.com/2015/day/5

d5 <- function(day = 5, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)

  # How many strings are nice?
  # at least three vowels (aeiou only)
  # at least one letter that appears twice in a row
  # does not contain the strings ab, cd, pq, or xy
  if (part == 1) {
    is_nice1 <- grepl("(?=(?:.*[aeiou]){3,})", input, perl = TRUE)
    is_nice2 <- grepl("([a-z])\\1", input, perl = TRUE)
    is_nice3 <- !grepl("ab|cd|pq|xy", input, perl = TRUE) 
    return(sum(is_nice1 & is_nice2 & is_nice3))
  }
  
  if (part == 2) {
    is_nice1 <- grepl("([a-z]{2}).*\\1", input, perl = TRUE)
    is_nice2 <- grepl("([a-z]).{1}\\1", input, perl = TRUE)
    return(sum(is_nice1 & is_nice2))
  }
}

# part 1
d5(example = TRUE,  part = 1) == 2
d5(example = FALSE, part = 1) == 255

# part 2
d5(example = FALSE, part = 2) == 55

# benchmark
microbenchmark::microbenchmark(
  d5(example = FALSE, part = 1), # 5.0 ms
  d5(example = FALSE, part = 2)  # 5.8 ms
)


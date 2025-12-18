# https://adventofcode.com/2015/day/2

d2 <- function(day = 2, year = 2015, example = TRUE, part = 1) {
  verbose <- example
  input <- 
    read_input(day, year, example) |> 
    strsplit("x") |> 
    lapply(as.integer)
  
  # length l, width w, and height h
  # How many total square feet of wrapping paper?
  if (part == 1) {
    # 2*l*w + 2*w*h + 2*h*l
    paper_sq <- sapply(input, function(x) {
      s1 <- x[1] * x[2]
      s2 <- x[2] * x[3]
      s3 <- x[1] * x[3]
      smallest <- min(s1, s2, s3)
      return(2*s1 + 2*s2 + 2*s3 + smallest)
    })
    return(sum(paper_sq))
  }
  # How many total feet of ribbon?
  if (part == 2) {
    volume_cf <- sapply(input, Reduce, f = "*")
    wrap_ft <- sapply(input, function(x) {
      ord <- sort(x) 
      return(2*ord[1] + 2*ord[2])
    })
    return(sum(wrap_ft) + sum(volume_cf))
  }
}

# part 1
d2(example = TRUE,  part = 1) == 101
d2(example = FALSE, part = 1) == 1588178

# part 2
d2(example = TRUE,  part = 2) == 48
d2(example = FALSE, part = 2) == 3783758

# benchmark
microbenchmark::microbenchmark(
  d2(example = FALSE, part = 1), #  3 ms
  d2(example = FALSE, part = 2)  # 40 ms
)


# https://adventofcode.com/2025/day/5

d5 <- function(day = 5, year = 2025, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example) # vector
  
  sep <- which(input == "") # find separator
  available <- 
    input[(sep+1):length(input)] |> 
    as.numeric()
  fresh_range <- 
    input[1:(sep-1)] |> 
    strsplit("-")
  
  # How many of the available ingredient IDs are fresh?
  if (part == 1) {
  available_fresh <- 
    lapply(fresh_range, function(x){
      (available >= as.numeric(x)[1] & available <= as.numeric(x)[2])
    }) 
  any_fresh <- Reduce(`+`, available_fresh) # >0 if in at least 1 range of fresh ids
  if (verbose) { message(any_fresh) }
  return(sum(any_fresh > 0))
  }
  
  # How many ingredient IDs are considered to be fresh 
  # according to the fresh ingredient ID ranges?
  if (part == 2) {
    ranges_num <- lapply(fresh_range, as.numeric)
    ranges <- ranges_num[ # order by start
      sapply(ranges_num, function(x) {x[1]}) |> 
        order()
    ]
    
    ranges_merged <- list()
    current_start <- ranges[[1]][1]
    current_stop  <- ranges[[1]][2]
    
    for (i in 2:length(ranges)) {
      start <- ranges[[i]][1]
      stop  <- ranges[[i]][2]
      
      if (start <= current_stop + 1) {
        # if overlapping or adjacent, extend
        current_stop <- max(current_stop, stop)
      } else {
        # if non-overlapping, save previous and start new
        ranges_merged <- c(ranges_merged, list(c(current_start, current_stop)))
        current_start <- start
        current_stop  <- stop
      }
    }
    
    # append final range
    ranges_merged <- c(ranges_merged, list(c(current_start, current_stop)))
    if (verbose) { print(ranges_merged) }
    
    sum_fresh_ids <- sum(sapply(ranges_merged, function(x) x[2] - x[1] + 1))
    return(sum_fresh_ids)
  }
}

# part 1
d5(example = TRUE, part = 1) == 3
d5(example = FALSE, part = 1) == 720
# part 2
d5(example = TRUE, part = 2) == 14
d5(example = FALSE, part = 2) == 357608232770687
# benchmark
microbenchmark(
  d5(example = FALSE, part = 1), # 3.2 ms
  d5(example = FALSE, part = 2)  # 1.2 ms
)

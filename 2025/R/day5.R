# part 1
fresh_ingredients <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day5/",
    verbose=FALSE
) {
  input <- readLines(paste0(path, file_name)) # vector
  sep <- which(input == "")                   # find separator
  available <- input[(sep+1):length(input)] |> as.numeric()
  fresh_range <- input[1:(sep-1)] |> strsplit("-")
  
  available_fresh <- 
    lapply(fresh_range, function(x){
      (available >= as.numeric(x)[1] & available <= as.numeric(x)[2])
    }) 
  any_fresh <- Reduce(`+`, available_fresh) # >0 if in at least 1 range of fresh ids
  if (verbose) { message(any_fresh) }
  available_fresh_sum <- sum(any_fresh > 0)
  message("Number of available fresh ingredients: ", available_fresh_sum)
}

fresh_ingredients("input_example.txt", verbose=T)
fresh_ingredients("input.txt")

# part 2
fresh_ranges <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day5/",
    verbose=FALSE
) {
  input <- readLines(paste0(path, file_name))     # vector
  sep <- which(input == "")                       # find separator
  ranges_raw <- input[1:(sep-1)] |> strsplit("-") # list of start and stop ids
  ranges_num <- lapply(ranges_raw, as.numeric)
  ranges <- ranges_num[                           # order by start
    sapply(ranges_num, function(x) {x[1]}) |> 
      order()
    ]
  
  # i <- 2
  ranges_combined <- NULL
  for (i in 1:length(ranges)) {
    if (verbose) { message("Range: ", i) }
    start <- ranges[[i]][1]
    stop <- ranges[[i]][2]
    if (i > 1) {
      prev_start <- ranges[[i-1]][1]
      prev_stop <- ranges[[i-1]][2]
      stopifnot(start >= prev_start, start <= stop)
    }
    
    if (i == 1){
      ranges_combined <- c(ranges_combined, ranges[i])
      if (verbose) { message("Added first range.") }
    } else if (stop <= prev_stop) {
      if (verbose) { message("Skipping.") }
    } else if (start > prev_stop) {
      if (verbose) { message("Added full range.") }
      ranges_combined <- c(ranges_combined, ranges[i])
    } else if (start <= prev_stop) {
      ranges_combined <- c(
        ranges_combined, 
        c(prev_stop + 1, stop) |> list()
        )
      if (verbose) { message("Added partial range.") }
    }
  }
  if (verbose) { print(ranges_combined) }

  ranges_diff <- lapply(ranges_combined, function(x){ x[2]-x[1]+1 })
  sum_fresh_ids <- Reduce(sum, ranges_diff)
  message("Number of fresh ids: ", sum_fresh_ids)
}

fresh_ranges("input_example.txt", verbose=T)
fresh_ranges("input.txt")

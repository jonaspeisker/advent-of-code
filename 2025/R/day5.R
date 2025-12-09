#### part 1 ####
d5p1 <- function(day=5, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines() # vector
  
  sep <- which(input == "") # find separator
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

d5p1(example=T)
d5p1(example=F)

#### part 2 ####
d5p2 <- function(day=5, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    readLines()                                   # vector
  
  sep <- which(input == "")                       # find separator
  ranges_raw <- input[1:(sep-1)] |> strsplit("-") # list of start and stop ids
  ranges_num <- lapply(ranges_raw, as.numeric)
  ranges <- ranges_num[                           # order by start
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
      # Overlapping or adjacent, extend
      current_stop <- max(current_stop, stop)
    } else {
      # Non-overlapping, save previous and start new
      ranges_merged <- c(ranges_merged, list(c(current_start, current_stop)))
      current_start <- start
      current_stop  <- stop
    }
  }
  
  # Append final range
  ranges_merged <- c(ranges_merged, list(c(current_start, current_stop)))
  if (verbose) { print(ranges_merged) }
  
  sum_fresh_ids <- sum(sapply(ranges_merged, function(x) x[2] - x[1] + 1))

  message("Number of fresh ids: ", sum_fresh_ids)
}

d5p2(example=T)
d5p2(example=F)

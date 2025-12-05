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

# part 1
fresh_ingredients("input_example.txt", verbose=T)
fresh_ingredients("input.txt")
# part 2
fresh_ingredients("input_example.txt", verbose=T)
fresh_ingredients("input.txt")

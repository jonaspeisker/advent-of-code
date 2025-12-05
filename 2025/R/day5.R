fresh_ingredients <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day5/",
    verbose=FALSE
) {
  input <- readLines(paste0(path, file_name))
  sep <- which(input == "")
  fresh_range <- input[1:(sep-1)] |> strsplit("-")
  fresh <- 
    lapply(fresh_range[3], function(x){
      # x <- as.numeric(x)
      x[1]:x[2]
      }) |> 
    unlist()
  available <- input[(sep+1):length(input)]
  
  available_fresh <- available %in% fresh
  available_fresh_sum <- sum(available_fresh)
  message("Number of available fresh ingredients: ", available_fresh_sum)
}

# part 1
fresh_ingredients("input_example.txt", verbose=T)
fresh_ingredients("input.txt")
# part 2
fresh_ingredients("input_example.txt", verbose=T)
fresh_ingredients("input.txt")

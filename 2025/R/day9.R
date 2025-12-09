largest_area <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day9/",
    verbose=FALSE
) {
  input <- readLines(paste0(path, file_name))
  # get unique pairs of points and parse coordinates
  combs <- 
    combn(input, 2) |> 
    apply(1:2, function(x) {
      strsplit(x, split=",") |> unlist() |> as.numeric()
      })
  # iterate over pairs of points
  areas <- sapply(1:dim(combs)[3], function(i) { # points in cols
  (abs(combs[1, 1, i] - combs[1, 2, i]) + 1) *   # x coord in row 1
    (abs(combs[2, 1, i] - combs[2, 2, i]) + 1)   # y coord in row 2
    })
  #return
  message("Size of largest area: ", max(areas))
}

largest_area()
largest_area("input.txt")

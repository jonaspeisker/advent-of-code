#### part 1 ####
d9p1 <- function(day = 9, example = TRUE, use_hull = TRUE) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.csv(header=F, col.names = c("x", "y"))
  
  if (use_hull){ 
    # get unique indeces of convex hull
    hull_ind <- 
      geometry::convhulln(as.matrix(input)) |>
      as.integer() |>
      unique()
    hull_points <- input[hull_ind, ]
  } else {
    hull_points <- input
  }
  
  if (verbose){
    plot(input, type = "l")
    points(hull_points, col="red")
  }
  
  # get unique pairs of indeces
  combs <- combn(nrow(hull_points), 2)
  # iterate over pairs (in cols)
  areas <- apply(combs, 2, function(col) { 
    p1 <- hull_points[col[1], ]; p2 <- hull_points[col[2], ] # select points by row index
    return((abs(p1$x - p2$x) + 1) * (abs(p1$y - p2$y) + 1))  # return area
  })
  
  return(max(areas))
}

# part 1
d9p1(example = TRUE) == 50
d9p1(example = FALSE) == 4771508457
# benchmark
microbenchmark(
  d9p1(example = FALSE, use_hull = TRUE),  # 0.2 s
  d9p1(example = FALSE, use_hull = FALSE), # 9.8 s
  times = 5
)

#### part 1 ####
d9p1 <- function(day=9, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.csv(header=F, col.names = c("x", "y"))
  
  if (verbose){ plot(input, type = "l") }
  # get unique pairs of indeces
  combs <- combn(nrow(input), 2)
  # iterate over pairs (in cols)
  areas <- apply(combs, 2, function(col) { 
    p1 <- input[col[1], ]; p2 <- input[col[2], ] # select points by row index
    return((abs(p1$x - p2$x) + 1) * (abs(p1$y - p2$y) + 1)) # return area
  })
  #return
  message("Size of largest area: ", max(areas))
}

d9p1(example=T)
d9p1(example=F)

#### part 1 with geometry library ####
d9p1_geo <- function(day=9, example=T) {
  verbose <- example
  input <- 
    get_file_name(day, example) |>
    read.csv(header=F, col.names = c("x", "y"))
  
  # get unique indeces of convex hull
  hull_ind <- 
    geometry::convhulln(as.matrix(input)) |>
    as.integer() |>
    unique()
  hull_points <- input[hull_ind, ]
  if (verbose){
    plot(input, type = "l")
    points(hull_points, col="red")
  }
  
  # Compute combinations only on the hull
  combs <- combn(nrow(hull_points), 2)
  # iterate over pairs (in cols)
  areas <- apply(combs, 2, function(col) { 
    p1 <- hull_points[col[1], ]; p2 <- hull_points[col[2], ] # select points by row index
    return((abs(p1$x - p2$x) + 1) * (abs(p1$y - p2$y) + 1))  # return area
  })
  #return
  message("Size of largest area: ", max(areas))
}

d9p1_geo(example=T)
d9p1_geo(example=F)


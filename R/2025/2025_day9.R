# https://adventofcode.com/2025/day/9

d9 <- function(day = 9, year = 2025, part = 1, example = TRUE) {
  verbose <- example
  input <- read_table(day, year, example, sep = ",") # matrix
  
  # What is the largest area of any rectangle you can make?
  if (part == 1) { 
    # largest rectangle must be formed with points on convex hull of all points
    hull <- input[chull(input), ]
    areas <- 
      outer(hull[, 1], hull[, 1], function(a, b) abs(a - b) + 1) * # x
        outer(hull[, 2], hull[, 2], function(a, b) abs(a - b) + 1) # y
    # combs <- combn(nrow(hull), 2)
    # areas <- apply(combs, 2, function(col) {
    #   p1 <- hull[col[1], ]
    #   p2 <- hull[col[2], ] # points index in rows 
    #   return((abs(p1[1] - p2[1]) + 1) * (abs(p1[2] - p2[2]) + 1))  # area
    # })
    return(max(areas))
  } 
  
  # What is the largest area of any rectangle you can make
  # within the polygon formed by all points?
  if (part == 2) {
    points <- 
      as.data.frame(input) |> 
      sf::st_as_sf(coords = 1:2)
    poly <- 
      points |> 
      sf::st_combine() |> 
      sf::st_cast("POLYGON") 

    combs <- combn(nrow(input), 2)
    areas <- apply(combs, 2, function(col) {
      p1 <- input[col[1], ]
      p2 <- input[col[2], ] # points index in rows 
      return((abs(p1[1] - p2[1]) + 1) * (abs(p1[2] - p2[2]) + 1))  # area
    })
    
    # consider rectangles in decreasing size
    areas_order <- areas |> order(decreasing = TRUE)
    max_area <- 0
    for (i in seq_len(length(areas))) {
      comb_col <- areas_order[i]
      comb_area <- areas[comb_col]
      
      box <- 
        points[combs[ , comb_col], ] |> 
        sf::st_bbox() |>
        sf::st_as_sfc()

      if (comb_area > max_area) {
        if (sf::st_covered_by(box, poly, sparse = FALSE)) {
          return(comb_area)
        }
      }
    }
  }
}

# part 1
d9(example = TRUE, part = 1) == 50
d9(example = FALSE, part = 1) == 4771508457

# part 2
d9(example = TRUE, part = 2) == 24
d9(example = FALSE, part = 2) == 1539809693

# benchmark
microbenchmark::microbenchmark(
  d9(example = FALSE, part = 1), # 2 ms
  # d9(example = FALSE, part = 2), # 89 s
  times = 1
)

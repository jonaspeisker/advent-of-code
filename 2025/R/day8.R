#### main ####
d8 <- function(day = 8, example = TRUE, part = 1) {
  verbose <- example
  input <- 
    get_file_name(day, example) |> 
    read.csv(header = FALSE)
  connections <- ifelse(example, 10, 1000)

  # get unique pairs of index 
  combs <- combn(nrow(input), 2)
  # compute distance
  dists <- apply(combs, 2, function(i){
    c("b1_ind" = i[1], "b2_ind" = i[2], # b1,2: index of junctions box 
      "dist" = eucl_dist(input[i[1],], input[i[2],]))
  }) |> 
  t() # rows to cols
  # sort by ascending distance
  dists_ord <- dists[order(dists[,"dist"]),]
  
  # iterate over pairs
  circuits <- list(c(dists_ord[1,"b1_ind"], dists_ord[1,"b2_ind"]))
  boxes <- 1:nrow(input)
  unconnected_boxes <- boxes[-1]
  for (i in 2:length(combs)) {
    if (verbose) {message(
      "Connection ", i, ":\n(", 
      dists_ord[i,"b1_ind"], ") ", 
      paste0(input[dists_ord[i,"b1_ind"],], collapse=","), 
      " with (", dists_ord[i,"b2_ind"], ") ", 
      paste0(input[dists_ord[i,"b2_ind"],], collapse=",")
    )}
    # next closest boxes
    con <- c(dists_ord[i,"b1_ind"], dists_ord[i,"b2_ind"])
    con_in_circuit <- sapply(circuits, function(x){
      con %in% x # boxes in rows, circuits in columns
    })
    circ_ind <- 
      apply(con_in_circuit, 2, function(x){ any(x) }) |> 
      which()   # connects to which circuits
    con_ind <- 
      apply(con_in_circuit, 1, function(x){ !any(x) }) |> 
      which()   # which boxes are not yet connected
    n_circuit <- length(circ_ind)     # to how many circuits
    if (n_circuit == 1) {             # add to circuit
      circuits[[circ_ind]] <- c(circuits[[circ_ind]], con[con_ind])
    } else if (n_circuit == 2) {      # merge circuits
      circuits[[circ_ind[1]]] <- 
        c(circuits[[circ_ind[1]]], circuits[[circ_ind[2]]], con[con_ind])
      circuits[[circ_ind[2]]] <- NULL # remove other circuit
    } else {                          # add new circuit
      circuits <- c(circuits, list(con))
    }
    
    # part 1: product of length of the three longest circuits
    if (part == 1 & i == connections) {
      # sort by descending length
      len_srt <- sapply(circuits, length) |> sort(decreasing = TRUE)
      # return product of three longest
      return(Reduce(`*`, len_srt[1:3]))
    }
    
    # part 2: product of the x coords of boxes that create one circuit of all boxes
    unconnected_boxes <- unconnected_boxes[
      !(unconnected_boxes %in% unlist(circuits))]
    if (length(unconnected_boxes) == 0 & length(circuits) == 1) {
      prod_p2 <- as.numeric(input[dists_ord[i,"b1_ind"], 1]) * 
        input[dists_ord[i,"b2_ind"], 1]
      return(prod_p2)
    } 
  } # end loop
} 

#### euclidean distance #### 
eucl_dist <- function(p, q) { # returns numeric
  p <- as.numeric(p); q <- as.numeric(q)
  sqrt((p[1]-q[1])^2 + (p[2]-q[2])^2 + (p[3]-q[3])^2)
}

#### run ####
# part 1
d8(example = TRUE, part = 1) == 40
d8(example = FALSE, part = 1) == 32103
# part 2
d8(example = TRUE, part = 2) == 25272
d8(example = FALSE, part = 2) == 8133642976
# benchmark
microbenchmark(
  d8(example = FALSE, part = 1),
  d8(example = FALSE, part = 2),
  times = 5
)
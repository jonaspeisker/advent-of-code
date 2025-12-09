# euclidean distance 
eucl_dist <- function(p, q) { # returns numeric
  p <- as.numeric(p); q <- as.numeric(q)
  sqrt((p[1]-q[1])^2 + (p[2]-q[2])^2 + (p[3]-q[3])^2)
}

get_circuits <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day8/",
    connections=10,
    verbose=FALSE
) {
  input <- read.csv(paste0(path, file_name), header=F)
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
    if (i == connections) {
      # sort by descending length
      len_srt <- sapply(circuits, length) |> sort(decreasing=TRUE)
      # return product of three longest
      message("Result for part 1: ", Reduce(`*`, len_srt[1:3]))
    }
    
    # part 2: product of the x coords of boxes that create one circuit of all boxes
    unconnected_boxes <- unconnected_boxes[
      !(unconnected_boxes %in% unlist(circuits))]
    if (length(unconnected_boxes) == 0 & length(circuits) == 1) {
      prod_p2 <- as.numeric(input[dists_ord[i,"b1_ind"], 1]) * 
        input[dists_ord[i,"b2_ind"], 1]
      message("Result for part 2: ", prod_p2)
      return(invisible(prod_p2))
    } 
  } # end loop
} 

get_circuits()
get_circuits("input.txt", connections=1000)

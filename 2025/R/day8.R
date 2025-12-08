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
  unconnected_boxes <- boxes[!(boxes %in% unlist(circuits))]
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
    in_circuit <- sapply(circuits, function(x){
      any(con %in% x)            # vector of circuits
    })
    c_ind <- which(in_circuit)   # connects to which circuits
    n_circuit <- length(c_ind)   # to how many circuits
    if (n_circuit == 1) {        # add to circuit
      circuits[[c_ind]] <- c(circuits[[c_ind]], con)
    } else if (n_circuit == 2) { # merge circuits
      circuits[[c_ind[1]]] <- c(circuits[[c_ind[1]]], circuits[[c_ind[2]]], con)
      circuits[[c_ind[2]]] <- NULL
    } else {                     # add new circuit
      circuits <- c(circuits, list(con))
    }
    
    # solve part 1
    if (i == connections) {
      circuits_unq <- lapply(circuits, unique) # remove duplicate boxes
      # sort by descending length
      len_srt <- sapply(circuits_unq, length) |> sort(decreasing=TRUE)
      # return product of three longest
      message("Result for part 1: ", Reduce(`*`, len_srt[1:3]))
    }
    
    # solve part 2
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

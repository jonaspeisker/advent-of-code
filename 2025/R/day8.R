#### euclidean distance ####
eucl_dist <- function(p,q) { # returns numeric
  p <- as.numeric(p); q <- as.numeric(q)
  sqrt((p[1]-q[1])^2 + (p[2]-q[2])^2 + (p[3]-q[3])^2)
}
# euclidean_dist(input[1,], input[2,]) |> class()

#### part 1 ####
get_circuits_p1 <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day8/",
    connections=10,
    verbose=FALSE
) {
  input <- read.csv(paste0(path, file_name), header=F)
  # get combinations
  combs <- combn(nrow(input), 2)
  # compute distance
  dists <- apply(combs, 2, function(i){
    c("b1_ind" = i[1], "b2_ind" = i[2], # b1,2: index of junctions box 
      "dist" = eucl_dist(input[i[1],], input[i[2],]))
  }) |> 
  t() # rows to cols
  # sort by ascending distance
  dists_ord <- dists[order(dists[,"dist"]),]
  
  circuits <- list(c(dists_ord[1,"b1_ind"], dists_ord[1,"b2_ind"]))
  for (i in 2:connections) {
    if (verbose) {
      message("Connection ", i, "\n", input[dists_ord[i,"b1_ind"],] |> paste0(collapse=","),
              " with ", input[dists_ord[i,"b2_ind"],] |> paste0(collapse=","))
    }
    con <- c(dists_ord[i,"b1_ind"], dists_ord[i,"b2_ind"])
    in_circuit <- sapply(circuits, function(x){
      any(con %in% x)
    })
    c_ind <- which(in_circuit) # to which circuits does the new connection connect
    n_circuit <- length(c_ind) # to how many circuits does the new connection connect
    if (n_circuit == 1) {
      circuits[[c_ind]] <- c(circuits[[c_ind]], con)
    } else if (n_circuit == 2) {
      circuits[[c_ind[1]]] <- c(circuits[[c_ind[1]]], circuits[[c_ind[2]]], con)
      circuits[[c_ind[2]]] <- NULL
    } else {
      circuits <- c(circuits, list(con))
    }
  }
  # circuits
  circuits_unq <- lapply(circuits, unique)
  len_srt <- sapply(circuits_unq, length) |> sort(decreasing=TRUE)
  
  message("The product is ", Reduce(`*`, len_srt[1:3]))
}

get_circuits_p1(connections=10, verbose=TRUE)
get_circuits_p1("input.txt", connections=1000)

#### part 2 ####
get_circuits_p2 <- function(
    file_name="input_example.txt", 
    path="../../Nextcloud/aoc25_inputs/day8/"
) {
  input <- read.csv(paste0(path, file_name), header=F)
  # get combinations
  combs <- combn(nrow(input), 2)
  # compute distance
  dists <- apply(combs, 2, function(i){
    c("b1_ind" = i[1], "b2_ind" = i[2], # b1,2: index of junctions box 
      "dist" = eucl_dist(input[i[1],], input[i[2],]))
  }) |> 
    t() # rows to cols
  # sort by ascending distance
  dists_ord <- dists[order(dists[,"dist"]),]
  
  circuits <- list(c(dists_ord[1,"b1_ind"], dists_ord[1,"b2_ind"]))
  # message("Connecting ", input[dists_ord[1,"b1_ind"],] |> paste0(collapse=","),
  #         " with ", input[dists_ord[1,"b2_ind"],] |> paste0(collapse=","))
  for (i in 2:connections) {
    # print(i)
    # message("Connecting ", input[dists_ord[i,"b1_ind"],] |> paste0(collapse=","),
    #         " with ", input[dists_ord[i,"b2_ind"],] |> paste0(collapse=","))
    # i <- 10
    con <- c(dists_ord[i,"b1_ind"], dists_ord[i,"b2_ind"])
    in_circuit <- sapply(circuits, function(x){
      any(con %in% x)
    })
    c_ind <- which(in_circuit) # to which circuits does the new connection connect
    n_circuit <- length(c_ind) # to how many circuits does the new connection connect
    if (n_circuit == 1) {
      circuits[[c_ind]] <- c(circuits[[c_ind]], con)
    } else if (n_circuit == 2) {
      circuits[[c_ind[1]]] <- c(circuits[[c_ind[1]]], circuits[[c_ind[2]]], con)
      circuits[[c_ind[2]]] <- NULL
    } else {
      circuits <- c(circuits, list(con))
    }
  }
  # circuits
  circuits_unq <- lapply(circuits, unique)
  len_srt <- sapply(circuits_unq, length) |> sort(decreasing=TRUE)
  
  message("The product is ", Reduce(`*`, len_srt[1:3]))
}


get_circuits(connections=10)
get_circuits("input.txt", connections=1000)



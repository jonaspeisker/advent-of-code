# https://adventofcode.com/2015/day/9

d9 <- function(day = 9, year = 2015, example = TRUE, part = 1, prune = FALSE) {
  verbose <- example
  input <- read_split(day, year, example, split = " ")
  edges <- data.frame(
    from = sapply(input, `[`, 1),
    to   = sapply(input, `[`, 3),
    dist = sapply(input, `[`, 5) |> as.integer()
  )
  
  # since graph is undirected, trips are possible in both directions
  edges <- rbind(
    edges,
    data.frame(
      from = edges$to,
      to   = edges$from,
      dist = edges$dist
    )
  )
  
  # named vector of distances 
  edge_dist <- edges$dist
  names(edge_dist) <- paste0(edges$from, edges$to)
  
  # permutations of all possible trips
  nodes <- unique(c(edges$from, edges$to))
  perm <- permutations(nodes)
  
  if (prune) { # remove reverse duplicates
    # R uses lexicographic comparison based on alphabetical order
    # Every route and its reverse compare unequal
    # Exactly one satisfies <
    keep <- apply(perm, 1, function(r) {
      paste0(r, collapse = "|") < paste0(rev(r), collapse = "|")
    })
    perm <- perm[keep, ]
  }
  
  # compute route lengths
  route_lengths <- apply(
    X = perm, MARGIN = 1,
    FUN = route_length, edge_dist = edge_dist
  )
  
  if (part == 1) {
    return(min(route_lengths))
  }
  if (part == 2) {
    return(max(route_lengths))
  }
}

# recursively create permutations of vector x
permutations <- function(x) {
  do.call(
    rbind,
    lapply(seq_along(x), function(i) {
      cbind(
        x[i],
        permutations(x[-i])
      )
    })
  )
}

# compute route length
route_length <- function(route, edge_dist) {
  keys <- paste0(route[-length(route)], route[-1])
  dists <- edge_dist[keys]
  
  if (any(is.na(dists))) Inf else sum(dists)
}

# part 1
d9(example = TRUE,  part = 1) == 605
d9(example = FALSE, part = 1) == 207

# part 2
d9(example = TRUE,  part = 2) == 982
d9(example = FALSE, part = 2) == 804

# benchmark
microbenchmark::microbenchmark(
  d9(example = FALSE, part = 1, prune = FALSE), # 1.4 s
  d9(example = FALSE, part = 1, prune = TRUE),  # 1.8 s
  times = 10
)


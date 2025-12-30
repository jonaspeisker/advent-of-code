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

message("Permutation helper loaded ✔")

#### depths first search: count paths ####

dfs_count_paths <- function(node, end_nodes, adj, memo) {
  
  # 1. Memo lookup
  if (exists(node, memo, inherits = FALSE)) {
    return(memo[[node]])
  }
  
  # 2. Base case: node is an end node
  if (node %in% end_nodes) {
    memo[[node]] <- 1
    return(1)
  }
  
  # 3. Recursive case
  children <- adj[[node]]
  
  # If no children, this is a dead end
  if (length(children) == 0) {
    memo[[node]] <- 0
    return(0)
  }
  
  total <- 0
  for (child in children) {
    total <- total + 
      dfs_count_paths(
        node = child, # new start node is child -> recursion
        end_nodes = end_nodes, 
        adj = adj, 
        memo = memo
      )
  }
  
  # 4. Store and return
  memo[[node]] <- total
  return(total)
}

message("DFS loaded ✔")


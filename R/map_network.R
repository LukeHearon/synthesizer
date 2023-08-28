map_network <- function(edges){
  source_nodes <- edges[!(edges$from %in% edges$to),"from"] %>%
    unlist() %>%
    unique()
  
  nodes <- unique(c(edges$from, edges$to))
  
  map <- rep(0, length(nodes))
  names(map) <- nodes
  
  map[names(map) %in% source_nodes] <- 0
  
  # loop control
  mapped <- F
  degree <- 1
  last_solved <- source_nodes
  
  # mapping loop
  while(mapped == F){
    vars_at_degree <- edges %>%
      filter(from %in% last_solved) %>%
      .$to %>%
      unique()
    
    if(length(vars_at_degree) == 0){mapped <- T}
    
    map[vars_at_degree] <- degree
    
    last_solved <- vars_at_degree # technically unnecessary but makes it more legible
    
    degree <- degree+1
  }
  
  return(map)
}

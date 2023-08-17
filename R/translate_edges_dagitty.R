translate_edges.dagitty <- function(edges_in){
  edges <- dagitty::edges(edges_in) %>%
    {data.frame(from = .$v, to = .$w)}
  
  return(edges)
}

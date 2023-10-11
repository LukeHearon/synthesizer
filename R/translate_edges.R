#' group_by.custom_class
#' 
#' @description Lorem ipsum
#' @export
translate_edges <- function(edges_in){
  UseMethod("translate_edges")
}

#' @export
#' @method translate_edges dagitty
translate_edges.dagitty <- function(edges_in){
  edges <- dagitty::edges(edges_in) %>%
    {data.frame(from = .$v, to = .$w)}
  
  return(edges)
}

#' @export
#' @method translate_edges character
translate_edges.character <- function(edges_in){
  # assumes all paths are diads represented by "->" which is a horrible assumption
  
  edges <- str_split( # break all the lines into elements
    string = edges_in,
    pattern = "\\n"
  ) %>%
    unlist() %>% # outputs a 1 element list, so unlsit
    .[str_detect(.,pattern = "->")] %>% # pull out directional arrows (this is the only kind with online dags)
    str_trim() %>%
    str_split(pattern = " -> ", simplify = T) %>% # break into two columns
    {data.frame(from = .[,1], to = .[,2])} # and make a named dataframe!
  return(edges)
}

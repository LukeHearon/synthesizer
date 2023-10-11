synth_network <- function(edges, sourceVals = NULL, n_obs = 100, noise = 0.1){
  if(class(edges) != "data.frame"){edges <- translate_edges(edges)}
  
  # if there are no path coefficients, create them
  if(is.null(edges$coefficient)){edges$coefficient <- rnorm(n = nrow(edges))}
  if(is.null(edges$sd)){edges$sd <- noise}

  map <- synthesizer::map_network(edges)
  nodes <- names(map)
  source_nodes <- names(map[map == 0])
  
  
  # Solve values
  #
  if(is.null(sourceVals)){
    sourceVals = data.frame(
      sapply(source_nodes, FUN = function(a){rnorm(n = n_obs)})
    )
  }

  solved_network <- apply(
    sourceVals,
    1,
    function(obs_in){
      # start values, automate later
      values <- map # hacky
      values[names(sourceVals)] <- obs_in

      for(iteration in 1:max(map)){
        solutions <- sapply(
          nodes[map == iteration],
          function(v){
            causes <- edges[edges$to == v,]

            causes$source_value <- values[match(x = causes$from, table = names(values))] %>%
              unlist()

            causes$effect <- rnorm(
              n = nrow(causes),
              mean = causes$coefficient,
              sd = causes$sd
            ) %>% 
              {.* causes$source_value}

            return(sum(causes$effect))
          }
        )

        values[names(solutions)] <- solutions
      }

      return(values)
    },

    simplify = F
  ) %>%
    bind_rows()


  return(solved_network)
}

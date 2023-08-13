generate_from_network <- function(edges, observations = NULL, n_obs = 100){
  if(is.null(edges$coefficient)){edges$coefficient <- rnorm(n = nrow(edges))}

  # Map degrees
  #
  ancestors <- edges[!(edges$from %in% edges$to),"from"] %>%
    unlist() %>%
    unique()

  variables <- unique(c(edges$from, edges$to))

  degrees <- rep(0, length(variables))
  names(degrees) <- variables

  degrees[names(degrees) %in% ancestors] <- 0

  # loop control
  mapped <- F
  degree <- 1 # I really don't like having "degrees" and "degree"
  last_solved <- ancestors

  # mapping loop
  while(mapped == F){
    vars_at_degree <- edges %>%
      filter(from %in% last_solved) %>%
      .$to %>%
      unique()

    if(length(vars_at_degree) == 0){mapped <- T}

    degrees[vars_at_degree] <- degree

    last_solved <- vars_at_degree # technically unnecessary but makes it more legible

    degree <- degree+1
  }


  # Solve values
  #
  if(is.null(observations)){observations = data.frame(sapply(ancestors, FUN = function(a){rnorm(n = n_obs)}))}

  solved_network <- apply(
    observations,
    1,
    function(obs_in){
      # start values, automate later
      values <- degrees # hacky
      values[names(observations)] <- obs_in

      for(iteration in 1:max(degrees)){
        solutions <- sapply(
          variables[degrees == iteration],
          function(v){
            causes <- edges[edges$to == v,]

            causes$value <- values[match(x = causes$from, table = names(values))] %>%
              unlist()

            causes$contribution <- causes$value * causes$coefficient

            return(sum(causes$contribution))
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

library(dplyr)
library(dagitty)
library(stringr)

#
# Setup
#

  dag_code <-  ' dag {
      bb="-7.289,-9.868,8.212,9.541"
      ancestor1 [pos="-5.652,-6.903"]
      ancestor2 [exposure,pos="-4.174,-8.251"]
      ancestor3 [pos="-3.540,4.103"]
      primary [pos="-3.668,-4.248"]
      primary2 [pos="-5.997,-5.462"]
      primary3 [pos="-2.842,1.249"]
      quaternary [exposure,pos="1.856,-5.686"]
      tertiary [pos="-0.330,-3.501"]
      v1 [latent,pos="0.086,0.001"]
      v2 [pos="-4.599,-1.289"]
      v3 [pos="-1.007,2.928"]
      v4 [pos="3.228,0.164"]
      v5 [pos="5.902,1.357"]
      v6 [pos="5.611,4.295"]
      v7 [pos="3.982,6.276"]
      v8 [pos="4.995,7.338"]
      v9 [outcome,pos="6.920,7.924"]
      ancestor1 -> primary
      ancestor2 -> ancestor1
      ancestor2 -> primary2
      ancestor3 -> primary3
      ancestor3 -> v3
      primary -> tertiary
      primary2 -> primary
      primary3 -> v1
      primary3 -> v2
      primary3 -> v3
      tertiary -> quaternary
      v1 -> tertiary
      v1 -> v4
      v2 -> primary
      v3 -> v1
      v4 -> v5
      v5 -> v6
      v6 -> v7
      v6 -> v8
      v7 -> v9
      v8 -> v9
    }
    '

  edges <- generator::translate_dag(dag_code)

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

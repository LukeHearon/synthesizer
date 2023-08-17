synth_linear <- function(effects_in, n_obs = 100, noise = 1){
  # should I impute missing levels? E.g., if I only see "TRUE" should I just impute FALSE = 0?

  # From the input effects, interpret the desired factors and their levels
  factors_in <- sapply( # pull the terms from each effect
    simplify = F,
    effects_in,
    function(e){e$term}
  ) %>%
    sapply(function(t){names(t)}) %>% # pull the names of all factors; following is some reshaping
    unlist() %>%
    unique() %>%
    sort() %>% # the names of all factors
    sapply(
      simplify = F,
      function(f){ # look inside of each effect to find the levels of each factor
        sapply(
          effects_in,
          function(e){
            e$term[names(e$term) == f]
          }
        ) %>%
          unlist() %>%
          unique() %>%
          sort()
      }
    )

  effects <- sapply(
    effects_in,
    FUN = function(effect_in){
      effect_in$value
    }
  )

  # check effects
  if(!is.numeric(effects)){stop("problem in effects; check that you have filled all values")}

  # synthesizer an artificial dataset
  data <- lapply(
    factors_in,
    function(f){
      if(length(f) > 1){sample(f, n_obs, replace = T)} else {
        runif(n_obs)
      }
    }
  ) %>%
    data.frame()


  # Create the ARRAY
  #

  # reshape our levels for use as dimensions in the ARRAY (should find a better name than just "factors")
  factors <- sapply(
    simplify = F,
    X = factors_in,
    FUN = function(factor_in){
      # check for redundant levels
      if(length(unique(factor_in)) < length(factor_in)){stop("Redundant factor levels detected")}

      # adjust input factor levels if they conflict with my nomenclature
      factor_in[factor_in == "nonInteracting"] <- "in_nonInteracting"

      if(is.logical(factor_in)){factor_out <- c(T, F, "nonInteracting")} # hmmm...this might never run because my TRUE/FALSE is getting coerced to character to append "nonInteracting"

        if(length(factor_in) == 1){
          if(factor_in == "numeric"){factor_out <- c("on", "nonInteracting")} else {warning("Categorical factor entered with only one level"); factor_out <- c(factor_in, "nonInteracting")} # appends "FALSE" to the end of the warning message
        } else {factor_out <- c(factor_in, "nonInteracting")}


      return(factor_out)
    }
  )

  # BUILD IT
  ARRAY <- array(
    data = 0,
    dim = sapply(factors, length),
    dimnames = factors
  )

  # Load the effects
  #

  coordinates_effectsIn <-  lapply(
    effects_in,
    FUN = function(effect_in){
      effect_in$term %>%
        sapply(as.character) %>%  # the only way I can think to preserve the names of the terms while coercing to character for later merging
        as.list() %>%
        as.data.frame()
    }
  ) %>%
    bind_rows()

  coordinates_effectsIn <- coordinates_effectsIn[sort(names(coordinates_effectsIn))] # alphabetize
  coordinates_effectsIn[is.na(coordinates_effectsIn)] <- "nonInteracting"
  coordinates_effectsIn[coordinates_effectsIn == "numeric"] <- "on"
  coordinates_effectsIn <- as.matrix(coordinates_effectsIn)

  ARRAY[coordinates_effectsIn] <- effects

  # Calculate response
  #

  colClasses <- sapply(data, class)

  get_effect <- function(observation_in){
    interactionDF <- observation_in
    interactionDF[colClasses == "numeric"] <- "on"
    interactionDF[2,] <- "nonInteracting"
    interactionDF <- expand.grid(interactionDF, stringsAsFactors = F)

    numericDF <- observation_in[rep(1, nrow(interactionDF)), colClasses == "numeric"] %>%
      as.data.frame() # needed in case there's one numeric, in which case rep() coerces to vector. I should just create my own rep.
    numericDF[interactionDF[colClasses == "numeric"] == "nonInteracting"] <- 1

    interactionDF$effect <- ARRAY[as.matrix(interactionDF)]
    interactionDF$numeric_coefficient <- apply(numericDF, 1, prod)

    interactionDF$component <- interactionDF$effect*interactionDF$numeric_coefficient

    response <- sum(interactionDF$component)

    return(response)
  }

  data$effect <- sapply(
    1:nrow(data), # because apply() vectorizes the rows, coercing numerics to characters
    function(r){get_effect(data[r,])}
  )

  data$noise <- rnorm(n = nrow(data), sd = noise)

  data$response <- data$effect + data$noise

  # Bingo bango bongo!
  #
  return(data)
}

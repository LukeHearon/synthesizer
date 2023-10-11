\name{synth_network}
\alias{synth_network}
\title{Synthesize a dataset from a directed network}

\description{
Synthesizes a dataset from an input directed network. The dataset will be a data frame with one column for each node. Starting values for the sources can be input as a data frame via the sourceVals argument; otherwise, they will be generated as ~Uniform(0,1).
}

\usage{
set_network(edges, observations = NULL, n_obs = 100)
}

\arguments{
  \item{edges}{
    An object specifying the edges of your DAG. This can be:
  
    \itemize{
      \item{
        a data frame with at least the following two columns: "from" holding the name of the causal node and "to" holding the name of the affected node
        optionally, a "coefficient" column can hold a numeric that specifies the strength of the path (e.g.,  coefficient of 2 means the affected node will inherit twice the value of the causal node)
      }
      \item{a character object where each line is a single edge in the format "A -> B"}
      \item{a dagitty object built by the package dagitty}
    }
  }
}


\examples{
  synth_network(
    edges = "A -> B"
  )
}

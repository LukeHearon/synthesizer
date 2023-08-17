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
  \edges{data.frame}{}
}


\examples{
hello()
}

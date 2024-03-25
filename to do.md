# Functions
# translate_edges
### QOL
- Allow arrows in any direction (`A -> B` or `B <- A`)
- Allow chaining edges (`A -> B -> C`, `Y <- X -> Z`)
- Allow no whitespace on path symbol (`A -> B` or `A->B`)

## synth_network
### QOL
- Allow user to specify means of source nodes by having NULL in to column of edges

### Computational
- Calculate variance covariance matrix and use MASS::mvrnorm for sampling?
- 
### Functional
- Allow categorical variables
- Simulate intervention on variables
- Allow custom functions along any edge

### Best practicals 
- Find a better name for "sourceVals"

# Documentation
- Make it...

# Whole-package
## Efficiency
- Remove dependence on dplyr

## Internals
- Create custom classes for synthesizer objects
networks
- Incorporate error testing

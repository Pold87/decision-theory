require(copula)
require(VineCopula)

## Convert a sample data matrix to data with uniform margins on [0, 1]
to.copuladata <- function(df) {
    rank(df)
}

## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../sample_data.csv", header = F)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(X, ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)



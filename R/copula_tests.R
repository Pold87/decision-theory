require(copula)
require(VineCopula)

## Convert a sample data matrix to data with uniform margins on [0, 1]
to.copuladata <- function(df) {
    rank(df)
}

## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../sample_data.csv", header = T)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(X, ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)


pairs.copuladata(X.cop)


# define 5-dimensional R-vine tree structure matrix
Matrix <- c(5, 2, 3, 1, 4,
            0, 2, 3, 4, 1,
            0, 0, 3, 4, 1,
            0, 0, 0, 4, 1,
            0, 0, 0, 0, 1)
Matrix <- matrix(Matrix, 5, 5)

RVC <- RVineStructureSelect(X.cop)

RVineTreePlot(data=NULL, RVM=RVC, tree=2,
              edge.labels=c("family","theotau"))

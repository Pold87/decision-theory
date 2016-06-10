require(copula)
require(VineCopula)

## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../sample_data.csv", header = T)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(X, ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)

pairs.copuladata(X.cop)

rvm <- RVineStructureSelect(X.cop)

cvm <- RVineStructureSelect(X.cop, type="CVine")

RVineTreePlot(data=NULL, RVM=cvm, tree=1,
              edge.labels=c("family","theotau"))


RVineVuongTest(X.cop, rvm, cvm)

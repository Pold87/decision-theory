require(copula)
require(VineCopula)


## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../sample_data.csv", header = T)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(as.matrix(X), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df[,c(3, 5)])

BiCopMetaContour(u1 = X.cop.df[,2], u2 = X.cop.df[,4],
                 bw = 1.2,size = 1000,
#                 levels = c(0.01,0.05,0.1,0.15,0.2),
                 par = 0,
                 family = "emp", main="Empirical with N=1000")

plot(x=X.cop.df[,2], y= X.cop.df[,4])

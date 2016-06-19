require(copula)
require(VineCopula)
require(matlab)

## Read R, G, B data and corresponding x, y-positions
## Read textons
X <- read.csv("../data/mat_train_hists_texton.csv", header = F)

## Read x, y-positions
y <- read.csv("../data/sample_data_full.csv", header = T)[, c(4,5)]

X <- cbind(X, y)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(as.matrix(X), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)

rvm <- RVineStructureSelect(X.cop, selectioncrit="logLik",
                            indeptest=TRUE, level=0.05,
                            familyset=c(1)
                            )

pairs.copuladata(X.cop)


## Read data (R, G, B, x, y)
X <- read.csv('../data/sample_data.csv')

## Empirical covariance matrix
S.X <- cov.wt(X, method = "ML")$cov

## Empirical inverse covariance matrix
K.X <- solve(S.X)

## Empiricial correlation matrix
C.X <- cor(X)

## Empiricial inverse correlation matrix
IC.X <- solve(C.X)

## Partial correlations
## Can be used as a starting point for Gaussian Model
PC.X <- cov2pcor(S.X)

## Check data
round(100 * PC.X)

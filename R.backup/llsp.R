require(gRbase)
require(gRim)

## Read data (R, G, B, x, y)
X <- read.csv('../data/sample_data.csv')

X <- X[, colSums(X^2) !=0]

## Empirical covariance matrix
S.X <- cov.wt(X, method = "ML")$cov

## Empirical inverse covariance matrix
K.X <- solve(S.X)

## Empiricial correlation matrix
C.X <- cor(X.train)

## Empiricial inverse correlation matrix
IC.X <- solve(C.X)

## Partial correlations
## Can be used as a starting point for Gaussian Model
PC.X <- cov2pcor(S.X)

## Check data
round(100 * PC.X)

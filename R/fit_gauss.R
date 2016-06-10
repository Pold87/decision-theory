require(gRbase)
require(gRim)
library(Rgraphviz)
library(VineCopula)
library(psych)


## Read data (R, G, B, x, y)
X <- read.csv('../sample_data.csv')

## Empirical covariance matrix
S.X <- cov.wt(X, method = "ML")$cov

## Empirical inverse covariance matrix
K.X <- solve(S.X)

## Check data
##round(10000 * K.X)

## Partial correlations
## Can be used as a starting point for Gaussian Model
PC.X <- cov2pcor(S.X)


## Full Gaussian model
sat.X <- cmod(~.^., data=X)

## AIC Gaussian model
aic.X <- stepwise(sat.X)

## BIC Gaussian model
bic.X <- stepwise(sat.X, k=log(nrow(X)))

plot(as(bic.X,"graphNEL"),"fdp")

# Fit using iterative proportional fitting
Xfit1 <- ggmfit(S.X, n=nrow(X),
                edgeList(as(sat.X, "graphNEL")))

Xfit2 <- ggmfit(S.X, n=nrow(X),
                edgeList(as(bic.X, "graphNEL")))



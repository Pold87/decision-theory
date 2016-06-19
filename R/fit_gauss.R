require(gRbase)
require(gRim)
library(Rgraphviz)
library(VineCopula)
library(psych)

# Compare two Graphical models
comparemodels <- function(m1,m2) {
     lrt <- m2$fitinfo$dev - m1$fitinfo$dev
     print(lrt)
     dfdiff <- m2$fitinfo$dimension[4] - m1$fitinfo$dimension[4]
     names(dfdiff) <- NULL
     list('lrt'=lrt, 'df'=dfdiff)
 }


## Read data (R, G, B, x, y)
X <- read.csv('../data/sample_data.csv')

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

#plot(as(bic.X,"graphNEL"),"fdp")


# Stepwise edge deletion
# Perform Chi/-squared test (tested)
test.X <- stepwise(sat.X,
                   search="all",
                   direction="backward",
                   details=2,
                   criterion="test")

# Look at correlation matrix plot
#pairs.panels(X, method='spearman')

# Fit using iterative proportional fitting
Xfit1 <- ggmfit(S.X, n=nrow(X),
                edgeList(as(sat.X, "graphNEL")))


# Fit using iterative proportional fitting
Xfit.test <- ggmfit(S.X, n=nrow(X),
                    edgeList(as(test.X, "graphNEL")))


Xfit2 <- ggmfit(S.X, n=nrow(X),
                edgeList(as(bic.X, "graphNEL")))


comparemodels(sat.X, test.X)

# Plot the resulting model
#plot(test.X)

# Delete edge from model
#satm1.X <- update(sat.X, list(dedge=~R:B))

# Manual chi squared test:
# Likelihood ration vs 0 (sat model)
#my.tab <- c(0, 32.43907)
#chisq.test(my.tab)


## Conditional distributions
R <- 100 
G <- 70
B <- 200

x_a <- c(R, G, B)

### Partition a matrix according to V_aa, V_ab, V_ba, V_bb

get_means <- function(X) {
    m <- colMeans(X)
    mu_a <- m[1:3]
    mu_b <- m[4:5]
    return(list('mu_a' = mu_a,
                'mu_b' = mu_b))
}

get_partitions <- function(S) {

    V_aa <- S[1:3, 1:3]
    V_ab <- S[1:3, 4:5]
    V_ba <- S[4:5, 1:3]
    V_bb <- S[4:5, 4:5]

    return(list('V_aa'=V_aa,
         'V_ab'=V_ab,
         'V_ba'=V_ba,
         'V_bb'=V_bb))
}

MS <- get_means(X)
VS <- get_partitions(S.X)

## Mean given R, G, B
E_b_given_a <- MS$mu_b + VS$V_ba %*% solve(VS$V_aa) %*% (x_a - MS$mu_a)

## Variance given R, G, B
var_b_given_a <- VS$V_bb - VS$V_ba %*% solve(VS$V_aa) %*% VS$V_ab



require(gRbase)
require(gRim)
library(stargazer)
source('extract_rgb.R')

read.from.csv <- FALSE

## Read data (R, G, B, x, y)
if (read.from.csv) {
    all.vals <- read.csv('data_stripes.csv')
} else {
    all.vals <- extract.rgb('stripes', write2csv = T)
}

# Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

## Split into training and test data
X.train <- all.vals[train.idx, c("R", "G", "B")]
X.test <- all.vals[test.idx, c("R", "G", "B")]
Y.train <- as.matrix(all.vals[train.idx, c("x", "y")])
Y.test <- as.matrix(all.vals[test.idx, c("x", "y")])

## Empirical variance matrix
var.X <- cov.wt(X.train, method = "ML")$cov

## Empirical inverse variance matrix
inv.var.X <- solve(var.X)

## Empiricial correlation matrix
cor.X <- cor(X.train)

## Empiricial inverse correlation matrix
inv.cor.X <- solve(cor.X)

## Partial correlations
## Can be used as a starting point for Gaussian Model
part.cor.X <- cov2pcor(var.X)

## Inverse correlation matrix for (X, Y) 
part.cor.XY <- solve(cor(cbind(X.train, Y.train)))

## Check data
round(100 * part.cor.X)

X.train.means <- colMeans(X.train)

## Calculate Y hat
E.Y <- as.matrix(X.train.means)

## Covariance between Y and X
cov.YX <- cov(Y.train, X.train)
B <- cov.YX %*% inv.var.X

### Verify using closed form solution

## Create design matrix (for interception)
ones <- rep(1, nrow(X.train))
X.train.design <- as.matrix(cbind(ones, X.train))

# Calculate regression coefficients
closed.B <- solve(t(X.train.design) %*% X.train.design) %*% t(X.train.design) %*% as.matrix(Y.train)

## Create design matrix for X.test
ones <- rep(1, nrow(X.test))
X.test.design <- as.matrix(cbind(ones, X.test))

## Make predictions on test set
preds <- X.test.design %*% closed.B

## Calculate mean squared error (MSE)
MSE <- (colSums((preds - Y.test) ^ 2)) / nrow(preds)

# Calulate R^2
sqrt(((500 * MSE) / (sum((Y.test - colMeans(Y.test))^2))))

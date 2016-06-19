require(gRbase)
require(gRim)
library(Rgraphviz)
require(psych)
require(ggplot2)
require(PerformanceAnalytics)
source('extract_rgb.R')

# Compare two Graphical models
comparemodels <- function(m1,m2) {
     lrt <- m2$fitinfo$dev - m1$fitinfo$dev
     print(lrt)
     dfdiff <- m2$fitinfo$dimension[4] - m1$fitinfo$dimension[4]
     names(dfdiff) <- NULL
     list('lrt'=lrt, 'df'=dfdiff)
 }

read.from.csv <- FALSE

## Read data (R, G, B, x, y)
if (read.from.csv) {
    all.vals <- read.csv('data_stripes.csv')
} else {
    all.vals <- extract.rgb('rainbow2', write2csv = T)
}

# Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

## Split into training and test data
X.train <- all.vals[train.idx, c("R", "G", "B")]
X.test <- all.vals[test.idx, c("R", "G", "B")]
Y.train <- as.matrix(all.vals[train.idx, c("x", "y")])
Y.test <- as.matrix(all.vals[test.idx, c("x", "y")])

## Z = (X, Y)
Z.train <- all.vals[train.idx, ]
Z.test <- all.vals[test.idx, ]

## Saturated Gaussian model
sat.Z <- cmod(~.^., data=Z.train)

# Stepwise edge deletion
# Perform Chi/-squared test (tested)
test.Z <- stepwise(sat.Z,
                   search="all",
                   direction="backward",
                   details=2,
                   criterion="test")

pdf('data.pdf')
chart.Correlation(Z.train,
                  histogram=TRUE,
                  pch="+",
                  method="spearman")
dev.off()


pdf('../../report/img/ggm.pdf')
plot(test.Z)
dev.off()


## Empirical variance matrix
var.Z <- cov.wt(Z.train, method = "ML")$cov

# Fit using iterative proportional fitting
Xfit1 <- ggmfit(var.Z, n=nrow(Z.train),
                edgeList(as(sat.Z, "graphNEL")))


D <- Xfit1$K
V <- solve(D)

chart.Correlation(Z.train,
                  histogram=TRUE,
                  pch="+",
                  method="spearman")


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

MS <- get_means(Z.train)
VS <- get_partitions(V)

## Make predictions on test set

preds <- matrix( , nrow(X.test), ncol(Y.test)) 

V.ba <- as.matrix(VS$V_ba)
V.aa <- as.matrix(VS$V_aa)
mu.a <- as.matrix(MS$mu_a)
mu.b <- as.matrix(MS$mu_b)

for (i in 1:nrow(X.test)) {
    ## Mean given R, G, B
    x.i <- t(as.matrix(X.test[i, ]))
    E.b.given.a <- mu.b +
        V.ba %*% solve(V.aa) %*% (x.i - mu.a)
    preds[i, ] <- E.b.given.a
}

## Error
err <- preds - Y.test



## Create design matrix (for interception)
ones <- rep(1, nrow(X.train))
X.train.design <- as.matrix(cbind(ones, X.train))


# Calculate regression coefficients
closed.B <- solve(t(X.train.design) %*% X.train.design) %*% t(X.train.design) %*% as.matrix(Y.train)

## Create design matrix for X.test
ones <- rep(1, nrow(X.test))
X.test.design <- as.matrix(cbind(ones, X.test))

## Make predictions on test set
preds.closed <- X.test.design %*% closed.B


comparemodels(sat.Z, test.Z)



## Variance given R, G, B
var_b_given_a <- VS$V_bb - VS$V_ba %*% solve(VS$V_aa) %*% VS$V_ab


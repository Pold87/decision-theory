require(gRbase)
require(gRim)
library(Rgraphviz)
require(psych)
require(ggplot2)
require(PerformanceAnalytics)
require(copula)
source('extract_rgb.R')

# Compare two Graphical models
comparemodels <- function(m1,m2) {
     lrt <- m2$fitinfo$dev - m1$fitinfo$dev
     print(lrt)
     dfdiff <- m2$fitinfo$dimension[4] - m1$fitinfo$dimension[4]
     names(dfdiff) <- NULL
     list('lrt'=lrt, 'df'=dfdiff)
 }

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

read.from.csv <- TRUE

## Read data (R, G, B, x, y)
if (read.from.csv) {
    all.vals <- read.csv('data_stripes.csv')
} else {
    all.vals <- extract.rgb('rainbow2', write2csv = T)
}

## Transform back to original data (640 x 480 images)
all.vals$x <- all.vals$x * 640
all.vals$y <- all.vals$y * 480

# Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

### Gauss with standard normal margins on R,G, B
all.uniform.RGB <- pobs(as.matrix(all.vals[, c("R", "G", "B")]))
R.gauss <- qnorm(all.uniform.RGB[, "R"])
G.gauss <- qnorm(all.uniform.RGB[, "G"])
B.gauss <- qnorm(all.uniform.RGB[, "B"])
all.trans <- cbind(R.gauss, G.gauss, B.gauss, all.vals[, c("x", "y")])
X.train.trans <- all.trans[train.idx, ]
X.test.trans <- all.trans[test.idx, ]

## Saturated Gaussian model
sat.trans <- cmod(~.^., data=X.train.trans)

# Stepwise edge deletion
# Perform Chi/-squared test (tested)
test.trans <- stepwise(sat.trans,
                   search="all",
                   direction="backward",
                   details=2,
                   criterion="test")

## Empirical variance matrix
var.trans <- var(X.train.trans)

# Fit using iterative proportional fitting
Xfit.trans <- ggmfit(var.trans, n=nrow(X.train.trans),
                edgeList(as(test.trans, "graphNEL")))


D.trans <- Xfit.trans$K
V.trans <- solve(D.trans)

MS.trans <- get_means(X.train.trans)
VS.trans <- get_partitions(V.trans)

## Make predictions on test set

preds.trans <- matrix( , nrow(X.test.trans), 2) 

V.ba <- as.matrix(VS.trans$V_ba)
V.aa <- as.matrix(VS.trans$V_aa)
mu.a <- as.matrix(MS.trans$mu_a)
mu.b <- as.matrix(MS.trans$mu_b)

for (i in 1:nrow(X.test.trans)) {
    ## Mean given R, G, B
    x.i <- t(as.matrix(X.test.trans[i, c("R.gauss", "G.gauss", "B.gauss")]))
    E.b.given.a <- mu.b +
        V.ba %*% solve(V.aa) %*% (x.i - mu.a)
    preds.trans[i, ] <- E.b.given.a
}

## Error
err <- preds.trans - X.test.trans[, c("x", "y")]
colnames(preds.trans) <- c("x", "y")
write.csv(preds.trans, "predictions_ggm.trans.csv", quote=F, row.names=F)

## Make predictions on training set

preds.trans.train <- matrix( , nrow(X.train.trans), 2) 

for (i in 1:nrow(X.train.trans)) {
    ## Mean given R, G, B
    x.i <- t(as.matrix(X.train.trans[i, c("R.gauss", "G.gauss", "B.gauss")]))
    E.b.given.a <- mu.b +
        V.ba %*% solve(V.aa) %*% (x.i - mu.a)
    preds.trans.train[i, ] <- E.b.given.a
}

## Error
err <- preds.trans.train - X.train.trans[, c("x", "y")]
colnames(preds.trans.train) <- c("x", "y")
write.csv(preds.trans.train, "predictions_ggm_train.trans.csv", quote=F, row.names=F)

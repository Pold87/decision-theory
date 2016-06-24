require(gRbase)
require(gRim)
library(stargazer)
library(knitr)
source('extract_rgb.R')

read.from.csv <- TRUE

## Read data (R, G, B, x, y)
if (read.from.csv) {
    all.vals <- read.csv('data_stripes.csv')
} else {
    all.vals <- extract.rgb('rainbow2', write2csv = T)
}

all.vals$x <- all.vals$x * 640
all.vals$y <- all.vals$y * 480

## Write for reporr
#write.csv(round(all.vals, 2), "for_report.csv", row.names=F, quote=F)

## Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

## Split into training and test data
X.train <- all.vals[train.idx, c("R", "G", "B")]
X.test <- all.vals[test.idx, c("R", "G", "B")]
Y.train <- as.matrix(all.vals[train.idx, c("x", "y")])
Y.test <- as.matrix(all.vals[test.idx, c("x", "y")])
all.train <- all.vals[train.idx, ]
all.test <- all.vals[test.idx, ]

## Fit using R's lm function
model <- lm(cbind(x, y) ~ R + G + B, data=all.train)

## Fit separately for POS_x and POS_y
model.x <- lm(x ~ R + G + B, data=all.train)
model.y <- lm(y ~ R + G + B, data=all.train)

## Manual calculations of model statistics
sigma.x <- sqrt(sum(model.x$res^2) / (500 - 4))
sigma.y <- sqrt(sum(model.y$res^2) / (500 - 4))
X.train.design <- as.matrix(cbind(rep(1, nrow(X.train)), X.train))
xtxi <- solve(t(X.train.design) %*% X.train.design)
errors.x <- sqrt(diag(xtxi)) * sigma.x
errors.y <- sqrt(diag(xtxi)) * sigma.y

## Empirical variance matrix
var.X <- var(X.train)

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
write.csv(preds, "predictions_llsp.csv", quote=F, row.names=F)


## Create residuals vs fitted plots
model.x.wo.g <- lm(x ~ R + B, data=all.train)
pdf("residuals_vs_fitted_x.pdf")
plot(fitted(model.x.wo.g), residuals(model.x.wo.g),
  xlab = "Fitted Values (x)", ylab = "Residuals", , cex.lab = 1.5)
abline(h=0, lty=2)
dev.off()

pdf("residuals_vs_fitted_y.pdf")
plot(fitted(model.y), residuals(model.y),
  xlab = "Fitted Values (y)", ylab = "Residuals", cex.lab = 1.5)
abline(h=0, lty=2)
dev.off()

## Create Q-Q norm plot
x.stdres = rstandard(model.x.wo.g)
pdf("qqnorm_x.pdf")
qqnorm(x.stdres, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores",
     main="",
     cex.lab=1.5) 
qqline(x.stdres)
dev.off()

y.stdres = rstandard(model.y)
pdf("qqnorm_y.pdf")
qqnorm(y.stdres, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores",
     main="",
     cex.lab=1.5) 
qqline(y.stdres)
dev.off()

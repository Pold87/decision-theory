require(copula)
require(VineCopula)
source("pairs.R")

## Read data (R, G, B, x, y)
X <- read.csv('../data/sample_data.csv')

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(as.matrix(X), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)

up <- function(x, y) {
  # lower panel: scatter plot (copula data) and correlation
  op <- par(usr = c(0, 1, 0, 1), new = TRUE)
  points(x, y, pch = 20, col = "gray")
  r <- cor(x, y, method = "spearman") # Spearman's rho
  txt <- format(x = r, digits = 3, nsmall = 3)[1]
  text(x = 0.5, y = 0.5, labels = txt, cex = 2 + abs(r) * 2, col = "blue")
  on.exit(par(op))
}

dp <- function(x, myx) {
  # diagonal panel: histograms (copula data)
  op <- par(usr = c(0, 1, 0, 1.5), new = TRUE)
    print(x)
  hist(myx, freq = FALSE, add = TRUE, col = "brown", border = "black", main = "")
  abline(h = 1, col = "black", lty = 2)
  on.exit(par(op))
}

pairs.copuladata(X.cop, upper.panel = up)

pdf('../img/pairs_copuladata.pdf', res=300, width=800)
pairs.copuladata(X.cop)
dev.off()

mypairs.copuladata(x=X.cop, myx=X, diag.panel = dp)

require(copula)
require(VineCopula)

## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../data/sample_data_fixed.csv", header = T)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(as.matrix(X), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)

# Normal copula
normal.cop <- normalCopula(dim=2)
fit.cop<- fitCopula(normal.cop, X.cop, method="ml")
# Coefficients
rho <- coef(fit.cop)
print(rho)
# Pseudo observations
plot(X.cop[,1], X.cop[,2],
     main="Pseudo/simulated observations: BLUE/RED",
     xlab="u",ylab="v",col="blue")
# Simulate data
set.seed(100)
u1 = rCopula(500,normalCopula(coef(fit.cop),dim=2))
points(u1[,1],u1[,2],col="red")

pairs.copuladata(X.cop)

RVC <- RVineStructureSelect(X.cop, familyset = c(1))

plot(RVC, edge.labels=c("family","theotau"), type=2)

contour(RVC)

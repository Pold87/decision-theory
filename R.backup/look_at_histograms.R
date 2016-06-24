require(psych)
require(copula)
require(VineCopula)

X <- read.csv('/home/pold/Documents/tudelft/advanced_decision/algo/sample_data.csv')

X.cop <- as.copuladata(X)

pairs.panels(X)

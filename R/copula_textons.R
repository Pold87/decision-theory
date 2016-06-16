require(copula)
require(VineCopula)
require(matlab)

## Read R, G, B data and corresponding x, y-positions
## Read textons
X <- read.csv("../data/mat_train_hists_texton.csv", header = F)

## Read x, y-positions
y <- read.csv("../data/sample_data_full.csv", header = T)[, c(4,5)]

X <- cbind(X, y)

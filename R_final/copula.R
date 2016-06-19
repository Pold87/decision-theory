require(copula)
require(VineCopula)
require(matlab)
require(psych)
require(lattice)
source('extract_rgb.R')
require(mefa)

read.from.csv <- TRUE

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
X.train <- all.vals[train.idx, ]
X.test <- all.vals[test.idx, ]
Y.train <- as.matrix(all.vals[train.idx, ])
Y.test <- as.matrix(all.vals[test.idx, ])

## Rank data (i.e., convert to CDF)
X.train.pseudo <- pobs(as.matrix(X.train), ties.method = 'random')
X.test.pseudo <- pobs(as.matrix(X.test), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.train.cop <- as.copuladata(X.train.pseudo)
X.test.cop <- as.copuladata(X.test.pseudo)

rvm <- RVineStructureSelect(X.train.cop, selectioncrit="logLik",
                            indeptest=TRUE, level=0.05,
                            familyset=NA)

pairs.copuladata(X.train.cop)

pairs.panels(X.train, hist.col="gray", pch="+", method="spearman")

pdf('../../report/img/tree1.pdf', res=300, width=800)
plot(rvm, edge.labels=c("family","theotau"), type=2, interactive=F, tree = 1)
dev.off()

pdf('../../report/img/contour_copula.pdf', res=300, width=800)
contour(rvm)
dev.off()


## Extract found copulas and plot using perspective plot


## Print contour plots of used copulas
fn <- "copula_plots/contours.pdf"
pdf(fn)
contour(rvm)
dev.off()


bicops <- list()

## Tree 1
## Iterate over family matrix
for (i in 1:(ncol(rvm$family) - 1)) {
  print(i)
  ## Construct copula
  cop <- BiCop(rvm$family[nrow(rvm$family), i],
               par = rvm$par[nrow(rvm$family), i],
               par2 = rvm$par2[nrow(rvm$family), i])
  fn <- sprintf("copula_plots/%d.pdf", i)
  bicops[[i]] <- cop
  pdf(fn)
  plot(cop, size=40)
  dev.off()
}

i = 1
fn <- sprintf("copula_plots/1.pdf")
pdf(fn)
plot(cop, size=40)
dev.off()



i = 4
## Construct copula
cop <- BiCop(rvm$family[nrow(rvm$family), i],
             par = rvm$par[nrow(rvm$family), i],
             par2 = rvm$par2[nrow(rvm$family), i])
fn <- sprintf("copula_plots/%d.pdf", i)
bicops[[i]] <- cop
pdf(fn)
plot(cop, size=40)
dev.off()


## Make predictions

construct.img <- function(val, x.max=640, y.max=480) {
  dim(val) <- c(x.max, y.max)
  return(val)
}

imagesc(img)


## TODO: print coordinates of maximum value

x.max = 640
y.max = 480
N.test = 10
xy <- expand.grid(1:x.max, 1:y.max)
xy.df <- data.frame(pobs(as.matrix(xy), ties.method = 'random'))

for (i in 1:N.test) {
  xy.df <- data.frame(pobs(as.matrix(xy), ties.method = 'random'))
  
  R = X.test.cop[i, "R"]
  G = X.test.cop[i, "G"]
  B = X.test.cop[i, "B"]
  
  R.all <- rep(R, nrow(xy))
  G.all <- rep(G, nrow(xy))
  B.all <- rep(B, nrow(xy))
  
  xy.df$R <- R.all
  xy.df$G <- G.all
  xy.df$B <- B.all
  
  colnames(xy.df) <- c("x", "y", "R", "G", "B")
  xy.df <- xy.df[c("R", "G", "B", "x", "y")]
  
  val <- RVinePDF(xy.df, rvm)
  img <- construct.img(val)
}


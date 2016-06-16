require(copula)
require(VineCopula)
require(matlab)

## Read R, G, B data and corresponding x, y-positions
X <- read.csv("../data/sample_data_full.csv", header = T)

## Rank data (i.e., convert to CDF)
X.cop.df <- pobs(as.matrix(X), ties.method = 'random')

## Convert to a copuladata object  (as used by the Vine Copula package)
X.cop <- as.copuladata(X.cop.df)

rvm <- RVineStructureSelect(X.cop, selectioncrit="logLik",
                            indeptest=TRUE, level=0.05,
                            familyset=c(1)
                            )

pairs.copuladata(X.cop)

pdf('../../report/img/tree1.pdf', res=300, width=800)
plot(rvm, edge.labels=c("family","theotau"), type=2, interactive=F, tree = 1)
dev.off()

pdf('../../report/img/contour_copula.pdf', res=300, width=800)
contour(rvm)
dev.off()

## Construct conditional copula
## This function takes a measurement R,G,B and creates a data frame for all possible
## x, y combinations
construct.conditional.df <- function(R, G, B, x.max=640, y.max=480) {
    xy <- expand.grid(1:x.max, 1:y.max)
    xy.df <- data.frame(pobs(as.matrix(xy), ties.method = 'random'))
    R.all <- rep(R, nrow(xy))
    G.all <- rep(G, nrow(xy))
    B.all <- rep(B, nrow(xy))
    xy.df$R <- R.all
    xy.df$G <- G.all
    xy.df$B <- B.all
    colnames(xy.df) <- c("x", "y", "R", "G", "B")
    xy.df <- xy.df[c("R", "G", "B", "x", "y")]
    return(xy.df)
}


construct.img <- function(val, x.max=640, y.max=480) {
    dim(val) <- c(x.max, y.max)
    return(val)
}


R <- 0.6
G <- 0.820
B <- 0.340
df <- construct.conditional.df(R, G, B)
val <- RVinePDF(df, rvm)
img <- construct.img(val)
imagesc(img)


generate.img.step1 <- function(x, y, R.max=255, G.max=255, B.max=255, step=10) {
    RGB <- expand.grid(seq(1, R.max, step),
                      seq(1, G.max, step),
                      seq(1, B.max, step))
    xy.df <- data.frame(pobs(as.matrix(RGB), ties.method = 'random'))
    x.all <- rep(x / 640, nrow(RGB))
    y.all <- rep(y / 480, nrow(RGB))
    xy.df$x <- x.all
    xy.df$y <- y.all
    colnames(xy.df) <- c("R", "G", "B", "x", "y")
    return(xy.df)
}


## Generate image

xmax <- 30
ymax <- 30
offset.x <- 100
offset.y <- 100
gen.img.R <- matrix(0, xmax, ymax)
gen.img.G <- matrix(0, xmax, ymax)
gen.img.B <- matrix(0, xmax, ymax)
for (x in 1:xmax) {
    for (y in 1:ymax) {
        per.pixel <- generate.img.step1(x + offset.x, y + offset.y)
        prop.pixel <- RVinePDF(per.pixel, rvm)
        argmax <- which.max(prop.pixel)
        colors <- per.pixel[argmax, ]
        gen.img.R[x,y] <- colors$R
        gen.img.G[x,y] <- colors$G
        gen.img.B[x,y] <- colors$B
    }
}
col <- rgb(gen.img.R, gen.img.G, gen.img.B)
dim(col) <- dim(gen.img.R)
library(grid)
grid.raster(col, interpolate = FALSE)

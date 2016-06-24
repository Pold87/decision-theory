require(copula)
#require(VineCopula)
require(matlab)
require(psych)
require(lattice)
require(fitdistrplus)
require(gRbase)
require(devtools)
source('extract_rgb.R')


dev_mode(on = T)
install_github("pold87/VineCopula")

library(VineCopula)

read.from.csv <- TRUE


## Read data (R, G, B, x, y)
if (read.from.csv) {
  all.vals <- read.csv('data_stripes.csv')
} else {
  all.vals <- extract.rgb('rainbow2', write2csv = FALSE)
}

# Transform back to original data (640 x 480 images)
all.vals$x <- all.vals$x * 640
all.vals$y <- all.vals$y * 480

## Rank RGB data and divide by N + 1
all.vals.pseudo <- pobs(as.matrix(all.vals), ties.method = 'random')

# Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

## Split into training and test data
X.train <- all.vals[train.idx, ]
X.test <- all.vals[test.idx, ]


## Calculate mean and standard deviation for the values x, y of the training set
mu.x <- mean(X.train$x)
mu.y <- mean(X.train$y)

std.x <- std(X.train$x)
std.y <- std(X.train$y)

pseudo.x.train <- pnorm(X.train$x, mu.x, std.x)
pseudo.y.train <- pnorm(X.train$y, mu.y, std.y)
pseudo.x.test <- pnorm(X.test$x, mu.x, std.x)
pseudo.y.test <- pnorm(X.test$y, mu.y, std.y)

X.train.pseudo <- cbind(all.vals.pseudo[train.idx, c("R", "G", "B")],
                        pseudo.x.train, pseudo.y.train)
X.test.pseudo <- cbind(all.vals.pseudo[test.idx, c("R", "G", "B")],
                       pseudo.x.test, pseudo.y.test)

colnames(X.train.pseudo) <- c("R", "G", "B", "x", "y")
colnames(X.test.pseudo) <- c("R", "G", "B", "x", "y")

## Convert to a copuladata object (as used by the Vine Copula package)
X.train.cop <- as.copuladata(X.train.pseudo)
X.test.cop <- as.copuladata(X.test.pseudo)

## Fit copula model to training dataset
rvm <- RVineStructureSelect(X.train.cop, selectioncrit="logLik",
                            indeptest=TRUE, level=0.05,
                            familyset=NA)

## Fit copula model to training dataset
rvm.sat <- RVineStructureSelect(X.train.cop, selectioncrit="logLik",
                            familyset=NA)


## Make predictions

construct.img <- function(val, x.max=640, y.max=480) {
  dim(val) <- c(x.max, y.max)
  return(val)
}

imagesc(img)


##file.remove("predictions.csv")
##file.remove("pdfs/*")
x.max = 640
y.max = 480
N.test = 500
xy <- expand.grid(1:x.max, 1:y.max)
## Convert to copula data
xy.df <- data.frame(x = pnorm(xy[, 1], mu.x, std.x),
                    y = pnorm(xy[, 2], mu.x, std.x))

## TEST SET
for (i in 1:N.test) {
  R = X.test.cop[i, "R"]
  G = X.test.cop[i, "G"]
  B = X.test.cop[i, "B"]
  #
  R.all <- rep(R, nrow(xy.df))
  G.all <- rep(G, nrow(xy.df))
  B.all <- rep(B, nrow(xy.df))
  #
  df <- xy.df
  df$R <- R.all
  df$G <- G.all
  df$B <- B.all
  #
  colnames(df) <- c("x", "y", "R", "G", "B")
  df <- df[c("R", "G", "B", "x", "y")]
  #
  val <- RVinePDF(df, rvm)
  ## Save density to file (for later visualization) 
  fn <- sprintf("pdfs/%d.csv", i)
  write.csv(val, fn, quote=FALSE,
            row.names=FALSE)
  ##
  ##img <- construct.img(val)
  ##
  ## Find point estimate
  idx <- which.max(val)
  row <- df[idx, ]
  pos.x <- qnorm(row$x, mu.x, std.x)
  pos.y <- qnorm(row$y, mu.y, std.y)
  ##
  ## Save to predictions file
  ##
  write.table(t(as.matrix(c(pos.x, pos.y))),
            file="predictions.csv", append = TRUE, sep=",",
            quote=FALSE, col.names=FALSE,
            row.names=FALSE)
  print(sprintf("preds are x: %f, y: %f, ground truth is x: %f, y: %f\n",
                pos.x, pos.y, X.test[i,"x"], X.test[i,"y"]))
  ##
}

N.train = 500
## TRAIN SET 
for (i in 1:N.train) {
  R = X.train.cop[i, "R"]
  G = X.train.cop[i, "G"]
  B = X.train.cop[i, "B"]
  #
  R.all <- rep(R, nrow(xy.df))
  G.all <- rep(G, nrow(xy.df))
  B.all <- rep(B, nrow(xy.df))
  #
  df <- xy.df
  df$R <- R.all
  df$G <- G.all
  df$B <- B.all
  #
  colnames(df) <- c("x", "y", "R", "G", "B")
  df <- df[c("R", "G", "B", "x", "y")]
  #
  val <- RVinePDF(df, rvm)
  ## Save density to file (for later visualization) 
  fn <- sprintf("pdfs_train/%d.csv", i)
  write.csv(val, fn, quote=FALSE,
            row.names=FALSE)
  ##
  ##img <- construct.img(val)
  ##
  ## Find point estimate
  idx <- which.max(val)
  row <- df[idx, ]
  pos.x <- qnorm(row$x, mu.x, std.x)
  pos.y <- qnorm(row$y, mu.y, std.y)
  ##
  ## Save to predictions file
  ##
  write.table(t(as.matrix(c(pos.x, pos.y))),
            file="predictions_cop_train.csv", append = TRUE, sep=",",
            quote=FALSE, col.names=FALSE,
            row.names=FALSE)
  print(sprintf("preds are x: %f, y: %f, ground truth is x: %f, y: %f\n",
                pos.x, pos.y, X.train[i,"x"], X.train[i,"y"]))
  ##
}


## Plots


## Show scattermatrix
pairs.copuladata(X.train.cop, method='spearman')

##pairs.panels(X.train, hist.col="gray", pch="+", method="spearman")

pdf('../../report/img/tree1.pdf', res=300, width=800)
plot(rvm, edge.labels=c("family","theotau"), type=2, interactive=F, tree = 1)
dev.off()

pdf('../../report/img/contour_copula.pdf', res=300, width=800)
contour(rvm)
dev.off()



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

## Extract found copulas and plot using perspective plot
bicops <- list()
i = 3
## Construct copula
cop <- BiCop(rvm$family[nrow(rvm$family), i],
             par = rvm$par[nrow(rvm$family), i],
             par2 = rvm$par2[nrow(rvm$family), i])
fn <- sprintf("copula_plots/%d.pdf", i)
bicops[[i]] <- cop
pdf(fn)
plot(cop, size=40)
dev.off()


## Print contour plots of used copulas
fn <- "copula_plots/contours.pdf"
pdf(fn)
contour(rvm)
dev.off()

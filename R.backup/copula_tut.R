library(MASS)

library(psych)

set.seed(100)

m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)
z <- mvrnorm(n, mu=rep(0, m),
             Sigma=sigma,empirical=T)


cor(z,method='spearman')
pairs.panels(z)

cree <- read.csv('/home/pold/Downloads/cree_r.csv',header=F)$V2
yahoo <- read.csv('/home/pold/Downloads/yahoo_r.csv',header=F)$V2

plot(cree,yahoo,pch='.')
abline(lm(yahoo~cree),col='red',lwd=1)
cor(cree,yahoo,method='spearman')

all.df <- cbind(cree,yahoo)
all.m <- as.matrix(all.df)

all.cpdata <- pobs(all.m)

u <- all.cpdata[,1]
v <- all.cpdata[,2]
selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula

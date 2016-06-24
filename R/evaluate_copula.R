########################################################################
## This function evaluates the predictions of model (saved as csv)
## with regard to MSE, MAE, and RMSE and corresponding standard
## deviations.
########################################################################

## Which model should be evaluated?
## 1: linear least squares
## 2: Gaussian graphical model
## 3: Vine Copula (mode)
## 4: Baseline (always 320, 240)
## 5: Vine Copula (simplified expectation)
variant <- 1
##
## Should the error be calculated on the train or on the testset?
validate.testset <- FALSE
##
if (validate.testset) {
    if (variant == 1) {
        preds <- read.csv('predictions_llsp.csv', header=TRUE)
    } else if (variant == 2) {
        preds <- read.csv('predictions_ggm.csv', header=TRUE)
    } else if (variant == 3) {
        preds <- read.csv('predictions_cop.csv', header=TRUE)
                                        #names(preds) <- c("x", "y")
    } else if (variant == 4) {
        preds <- read.csv('predictions_baseline.csv', header=TRUE)
    } else if (variant == 5) {
        preds <- read.csv('predictions_exp_cop.csv', header=TRUE)
    } else if (variant == 6) {
        preds <- read.csv('predictions_ggm.trans.csv', header=TRUE)
    } else {
        stop('Unknown variant')
    }
} else {
    if (variant == 1) {
        preds <- read.csv('predictions_llsp_train.csv', header=TRUE)
    } else if (variant == 2) {
        preds <- read.csv('predictions_ggm_train.csv', header=TRUE)
    } else if (variant == 3) {
        preds <- read.csv('predictions_cop_train.csv', header=TRUE)
    } else if (variant == 4) {
        preds <- read.csv('predictions_baseline.csv', header=TRUE)
    } else if (variant == 6) {
        preds <- read.csv('predictions_ggm_train.trans.csv', header=TRUE)        
    } else {
        stop('Unknown variant')
    }
}

# Create training and test indices
train.idx <- 1:500
test.idx <- 501:1000

actual <- read.csv('data_stripes.csv')
## Transform back to original data (640 x 480 images)
actual$x <- actual$x * 640
actual$y <- actual$y * 480
## Split into train and testset
actual.train <- actual[train.idx, ]
actual.test <- actual[test.idx, ]

## Calculate training or test error?
if (validate.testset) {
    actual <- actual.test
} else {
    actual <- actual.train
}

# Calculate squared errors
diffs <- data.frame(diff.x = (actual$x - preds$x)^2,
                    diff.y = (actual$y - preds$y)^2)

# Calculate squared errors
abs.diffs <- data.frame(diff.x = abs(actual$x - preds$x),
                    diff.y = abs(actual$y - preds$y))


# Calculate mean squared error (MSE)
MSE <- colMeans(diffs)
RMSE <- sqrt(MSE)

## Calculate Mean Absolute Error (MAE)
MAE <- colMeans(abs.diffs)
SD.x <- sd(abs.diffs$diff.x)
SD.y <- sd(abs.diffs$diff.y)

## Calculate R^2 = var(Y_hat) / var(Y) 
R2 <- diag(var(preds)) / diag(var(actual[, c("x", "y")]))

###########
## PLOTS ##
###########
res <- actual[, c("x", "y")] - preds

write.csv(res, "resids_cop.csv", quote=F, row.names=F)
####
## Create residuals vs fitted plots
####
fn.rf.x <- sprintf("residuals_vs_fitted_x_%d.pdf", variant)
pdf(fn.rf.x)
plot(preds$x, res$x,
  xlab = "Fitted Values (x)", ylab = "Residuals", , cex.lab = 1.5)
abline(h=0, lty=2)
dev.off()

fn.rf.y <- sprintf("residuals_vs_fitted_y_%d.pdf", variant)
pdf(fn.rf.y)
plot(preds$y, res$y,
  xlab = "Fitted Values (y)", ylab = "Residuals", cex.lab = 1.5)
abline(h=0, lty=2)
dev.off()

####
## Create fitted vs actual plot
####
fn.ra.x <- sprintf("residuals_vs_actual_x_%d.pdf", variant)
pdf(fn.ra.x)
plot(preds$x, actual$x,
  xlab = "Fitted Values (x)", ylab = "Actual Values(x)", , cex.lab = 1.5)
abline(0, 1, lty=2)
dev.off()

fn.ra.y <- sprintf("residuals_vs_actual_y_%d.pdf", variant)
pdf(fn.ra.y)
plot(preds$y, actual$y,
  xlab = "Fitted Values (y)", ylab = "Actual Values(y)", cex.lab = 1.5)
abline(0, 1, lty=2)
dev.off()


####
## Create Boxplot Residuals
####
fn.ra.x <- sprintf("boxplot_%d.pdf", variant)
pdf(fn.ra.x)
boxplot(res, cex.lab = 1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(h=0, lty=2)
dev.off()

## Maybe for Linear regression only
## Create Q-Q norm plot
x.stdres = rstandard(model.x.wo.g)
fn.qq.x <- sprintf("qqnorm_x_%d.pdf", variant)
pdf(fn.qq.x)
qqnorm(x.stdres, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores",
     main="",
     cex.lab=1.5) 
qqline(x.stdres)
dev.off()

y.stdres = rstandard(model.y)
fn.qq.y <- sprintf("qqnorm_y_%d.pdf", variant)
pdf(fn.qq.y)
qqnorm(y.stdres, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores",
     main="",
     cex.lab=1.5) 
qqline(y.stdres)
dev.off()

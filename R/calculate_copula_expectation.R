N = 500

x.max = 640
y.max = 480
N.test = 500
xy <- expand.grid(1:x.max, 1:y.max)

## The function makes the simplificiation that the distributed is
## constrained to 640 x 480.
# An improvement would be to also model the joint distribution for RGB


for (i in 1:N) {
    fn <- sprintf("pdf.bk/%d.csv", i)
    vals <- read.csv(fn)
    vals <- vals / sum(vals)
    exp.x <- sum(vals * xy[, 1])
    exp.y <- sum(vals * xy[, 2])
    write.table(t(as.matrix(c(exp.x, exp.y))),
            file="predictions_exp_cop.csv", append = TRUE, sep=",",
            quote=FALSE, col.names=FALSE,
            row.names=FALSE)}

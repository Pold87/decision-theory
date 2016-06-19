require(png)

## FINAL VERSION

## This function reads N image patches from a specified directory,
## extracts the mean red (R), green (G), and blue (B) value per patch
## and writes the results to a CSV file with corresponding x,
## y-position of the patch.

## base.name: base image folder
## map.name: name of map, e.g. 'pencils';
## N: Number of samples
## write2csv: Boolean; save output in CSV file

extract.rgb <- function(map.name, base.name='/home/pold/Documents/map_evaluation',
                        N=1000, write2csv=FALSE) {

    ## path of image patches
    p = sprintf('%s/%s', base.name, map.name);

    ## Path of x, y positions
    pos.name <- sprintf('%s/targets.csv', p);
    pos <- read.csv(pos.name)
    pos <- pos[, c("x", "y")]
    pos$x <- pos$x / 640
    pos$y <- pos$y / 480

    ## Initialize mean matrix
    means <- matrix(, N, 3)

    ## Iterate over images
    for (i in 0:(N-1)) {
        img.path <- sprintf('%s/img_%05d.png', p, i)
        print(img.path)
        I <- readPNG(img.path)
        means[i+1, ] <- colMeans(I, dims=2)
    }

    ## Write to file
    means <- data.frame(means)
    colnames(means) <- c("R", "G", "B")

    all.vals <- cbind(means, pos[1:N, ])

    # Save output in CSV file
    if (write2csv) {
        write.csv(all.vals, "data_stripes.csv", quote=FALSE, row.names=FALSE)
    }

    return(all.vals)

}

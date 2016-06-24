# Advanced Topics in Decision Theory

This repository contains the material for the course Advanced Topics
in Decision Theory (TU Delft). In the course, the dependence structure
between color features of image patches and their x, y-coordinates in
a larger map image is analyzed using the linear least squares
predictor, graphical Gaussian models, and a vine copula.

The script for extracting the mean red, green, and blue value is
R/extract_rgb.R. The resulting dataset can be found in
dataset.csv. The evaluation of the different models was performed
using the script R/evaluate.R.

## Linear Least Squares

File: R/llsp.R

## Graphical Gaussian Model

Files: R/gauss.R (non-normal margins) and R/gauss_trans.R (transformed normal margins)

## Vine Copula

File: R/copula.R

## Evaluation

File: R/evaluate.R



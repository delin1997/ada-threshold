library(R.matlab)
library(rgl)
source("ada_threshold_3d.R")

volume <- readMat('sample.mat')$vol
label  <- readMat('true_label.mat')$md
label[label!=0] = 1
true_plot <- which(label==1, arr.ind = TRUE)
plot3d(true_plot, col = rgb(0,0,1,0.2))


ada <- ada_threshold_3d(volume)
ada_plot <- which(ada==1, arr.ind = TRUE)
plot3d(ada_plot, col = rgb(0,0,1,0.2))


TP <- sum(label==1&ada==1)/sum(label==1)
FP <- 1-TP
TN <- sum(label==0&ada==0)/sum(label==0)
FN <- 1-TN

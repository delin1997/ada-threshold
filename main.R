library(png)
library(grid)
source("ada_threshold.R")
image1 <- readPNG("image1.png")[,,1]
ada1 <- ada_threshold(data = image1)
# writePNG(ada1, "ada_image1.png")
grid.raster(image1)
grid.raster(ada1)
image2 <- readPNG("image2.png")[,,1]
ada2 <- ada_threshold(data = image2)
# writePNG(ada1, "ada_image2.png")
grid.raster(image2)
grid.raster(ada2)

ada_threshold <- function(data){
  w <- nrow(data)
  h <- ncol(data)
  t <- 15
  s <- round(w/16)
  intImg <- matrix(0, nrow = w, ncol = h)
  out <- matrix(0, nrow = w, ncol = h)
  for (i in 1:w) {
    summation <- 0
    for (j in 1:h) {
      summation <- summation + data[i, j]
      if(i==1){
        intImg[i, j] <- summation
      }else{
        intImg[i, j] <- intImg[i-1, j] + summation
      }
    }
  }
  for (i in 1:w) {
    for (j in 1:h) {
      x1 <- max(i - s, 1)
      x2 <- min(i + s, w)
      y1 <- max(j - s, 1)
      y2 <- min(j + s, h)
      count <- (x2-x1)*(y2-y1)
      summation <- intImg[x2, y2]-intImg[x2, y1]-intImg[x1, y2]+intImg[x1, y1]
      if((data[i, j]*count)<=(summation*(100-t)/100)){
        out[i, j] <- 0
      }else{
        out[i, j] <- 1
      }
    }
  }
  return(out)
}
ada_threshold_3d <- function(data){
  dimension <- dim(data)
  w <- dimension[1]
  h <- dimension[2]
  d <- dimension[3]
  t <- 15
  s <- round(w/16)
  intImg <- array(0, dim = dimension)
  out <- array(0, dim = dimension)
  for (k in 1:d) {
    for (i in 1:w) {
      summation <- 0
      for (j in 1:h) {
        summation <- summation + data[i, j, k]
        if(k==1){
          if(i==1){
            intImg[i, j, k] <- summation
          }else{
            intImg[i, j, k] <- intImg[i-1, j, k] + summation
          }
        }else{
          if(i==1){
            intImg[i, j, k] <- intImg[i, j, k-1] + summation
          }else{
            intImg[i, j, k] <- intImg[i-1, j, k] + intImg[i, j, k-1] - intImg[i-1, j, k-1] + summation
          }
        }

      }
    }
  }
  for (k in 1:d) {
    for (i in 1:w) {
      for (j in 1:h) {
        x1 <- max(i - s, 1)
        x2 <- min(i + s, w)
        y1 <- max(j - s, 1)
        y2 <- min(j + s, h)
        z1 <- max(k - s, 1)
        z2 <- min(k + s, d)
        count <- (x2-x1)*(y2-y1)*(z2-z1)
        summation <- intImg[x2, y2, z2]-intImg[x2, y2, z1]-intImg[x2, y1, z2]-intImg[x1, y2, z2]+intImg[x2, y1, z1]+intImg[x1, y2, z1]+intImg[x1, y1, z2]-intImg[x1, y1, z1]
        if((data[i, j, k]*count)<=(summation*(100-t)/100)){
          out[i, j, k] <- 0
        }else{
          out[i, j, k] <- 1
        }
      }
    }
  }
  return(out)
}
#####**********function return central refuge***********####
#need col and row number
#xcen,ycen: dimention of the centre wanted
centre <- function(col, row, xcen, ycen) {
  xcentre <- (col / 2 - xcen / 2 + 1):(col / 2 + xcen / 2)
  ycentre <- (row / 2 - ycen / 2 + 1):(row / 2 + ycen / 2)
  centre <- NULL
  for (i in 1:length(xcentre)) {
    for (j in 1:length(ycentre)) {
      xy <- ((xcentre[i] - 1) * col + ycentre[j])
      centre <- c(centre, xy)
    }
  }
  return(centre)
}
####*********end of the function**********#########
refuge_centre <- centre(col, row, xcen, ycen)






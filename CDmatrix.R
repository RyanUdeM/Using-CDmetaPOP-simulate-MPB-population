#Function create the "euclidean" cost distance matrices between patches (n*n)
#n = col*row  is also the size of the landscape

setwd("C:/Users/Guest/Desktop/Ryan/2018 summer")

#landscape size
col<- 81
row<- 81



cdMatrix<-function(col,row)
{
  XCOORDm<-rep(1:col,times=row)
  YCOORDm<-rep(1:row,each=col)
  COORDm<-as.data.frame(cbind(XCOORDm,YCOORDm))
  euclid<-round(as.matrix(dist(COORDm[,1:2],method="euclidean")),2)
  write.table(euclid,file="Cdmatrix.csv",row.names=FALSE,col.names=FALSE,sep=",")
  
  #create burn in cost distance matrix
  #patchClosed<-matrix(99,col*row,col*row)
  #write.table(patchClosed,file="PatchClosed.csv",row.names=FALSE,col.names=FALSE,sep=",")
  
}

cdMatrix(col,row)
##Ryan 2018 summer
##this code is based on Paul's demographic code
##we use the output file "summary_popAllTime.csv" of CDmetaPOP, demostrate the density in grid cells at the wanted timesteps


#https://stackoverflow.com/questions/5638462/r-image-of-a-pixel-matrix

#install.packages("plotrix")
library(plotrix)

directory<-"C:/Users/Guest/Desktop/Ryan/2018 summer/demographic"
setwd(directory)

#we have to give the landscape size:nxn. this has to be the same as size as the grid used for the simulation
n <- 21

#min and max values in the ladder
xmin <- 0
xmax <- 30

#create new folder to save figures
dir.create("Demographics")
path<-paste(directory,"Demographics",sep="/")


#load simulation data, here we use summary_popAllTime.csv
summary_pop<-read.table("summary_popAllTime.csv",sep=",",header=TRUE,row.names = NULL)
summary<-summary_pop[,c(1,4)] #colonne 4 is the N_Initial of every output timestep
popu<-summary[-seq(2,nrow(summary),2),,drop=FALSE] #use only the data of output years
write.table(popu[,2],file="indivs.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)

pop<-read.table("indivs.csv",sep="|")

popSize<-NULL #vector that has the population size of all timesteps
for(i in 1:nrow(popu)){
  popSize<-c(popSize,pop[i,1])
}

plot(popSize,type="o")




#data[year, pixel] gives the population in the pixel of every output year
data<-cbind(popu[,1],pop[,2:(ncol(pop)-1)])
setwd(path)

#for each output year (generation)

for(i in 1:nrow(data)){
  demo<-data[i,]
  name<-paste("Generation",i,"png",sep=".")
  png(filename=name)
  
  
  #total years = nrow(data)
  #matrix represent the landscape
  x<-NULL
  #for each pixel
  for(j in 2:(n*n+1)){
  
     x<-c(x,data[i,j])
   }

  x<-matrix(x,nrow=n)

  #Generate the palette for the matrix and the legend.  Generate labels for the legend
  palmat <- color.scale(x, c(1, 0), c(1, 0), c(0.96, 1))
  palleg <- color.gradient(c(1, 0), c(1, 0), c(0.96, 1), nslices=100)
  lableg <- c(formatC(xmin, format="f", digits=2), 
              formatC(1*(xmax-xmin)/4, format="f", digits=2), 
              formatC(2*(xmax-xmin)/4, format="f", digits=2), 
              formatC(3*(xmax-xmin)/4, format="f", digits=2), 
              formatC(xmax, format="f", digits=2))

  #Set up the plot area and plot the matrix
  par(mar=c(5, 5, 5, 5))
  color2D.matplot(x, cellcolors=palmat, main=paste(n, " X ", n, " kMax = ",xmax, sep=""),
                  show.values=0, vcol=rgb(0,0,0), axes=FALSE, vcex=0.5)    #show.values=2  to show numbers in pixels
  axis(1, at=seq(1, n, 1)-0.5, labels=seq(1, n, 1), tck=-0.01, padj=-1)

  #In the axis() statement below, note that the labels are decreasing.  This is because
  #the above color2D.matplot() statement has "axes=FALSE" and a normal axis()
  #statement was used.
  axis(2, at=seq(1, n, 1)-0.5, labels=seq(n, 1, -1), tck=-0.01, padj=0.7)

  #Plot the legend
  pardat <- par()
  color.legend(pardat$usr[2]+0.5, 0, pardat$usr[2]+1.5, pardat$usr[2], paste(" ", lableg, sep=""), palleg, align="rb", gradient="y", cex=0.7, line =2)

  dev.off()
  
}
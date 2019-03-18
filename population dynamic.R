

directory<-"C:/Users/Guest/Desktop/Ryan/2018 summer/demographic"
setwd(directory)

#############################################################
#############################################################
####### part 1: import summary_popAlltime.csv ###############
#############################################################
#############################################################


#load simulation data, here we use summary_popAllTime.csv
summary_pop<-read.table("summary_popAllTime.csv",sep=",",header=TRUE,row.names = NULL)




#extract timestep and population size
popSummary<-summary_pop[,c(1,4)] #column 1 is timestep, column 4 is the N_Initial

popu<-popSummary[-seq(2,nrow(popSummary),2),,drop=FALSE] #use only the data of the even number timesteps
write.table(popu[,2],file="indivs.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
pop<-read.table("indivs.csv",sep="|")
popSize<-NULL #vector that has the population size of all timesteps
for(i in 1:nrow(popu)){
  popSize<-c(popSize,pop[i,1])
}



#chosen timesteps (even timesteps)
write.table(popu[,1],file="time.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
time<-read.table("time.csv")
timeSteps<-NULL #vector that has the population size of all timesteps
for(i in 1:nrow(popu)){
  timeSteps<-c(timeSteps,time[i,1])
}

##############################################################
##############################################################
#### part 2: find peaks and valleys and convert###############
##############################################################
##############################################################


#########################################
####*****find peaks fonction***##########
###notice:a 'peak' is defined as a local maxima with m points either side of it being smaller than it. 
###hence, the bigger the parameter m, the more stringent is the peak funding procedure
find_peaks <- function (x, m = accu ){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
}

####*****find_peaks(-x) can be used to find valleys****######
############################################################
find_valleys <- function (x,m = accu){
  vals<-find_peaks(-x,m)
  vals
}

pks<-find_peaks(popSize)
vals<-find_valleys(popSize)


#function that delete fake pks (which cause by 2 continuous kmax or k=0 in the k string)
#remove pk if it is in the m steps following the previous peak.
truepks<-function(pks,m = accu){
  fakepks<-NULL
  for (i in 2:length(pks)){
    ss<-pks[i]-pks[i-1]
    if (ss<=m) {
      fakepks<-c(fakepks,i)
    }
  }
  if(length(fakepks)>0){
    pks<-pks[-fakepks]
    pks
  }else{
    pks
  }
  #pks <-pks[(length(pks)-10):(length(pks)-1)]
}

pks<-truepks(pks,m = accu)
vals<-truepks(vals,m = accu)

#if population grows continuously before 1st peak, we consider the 1 genaration is the first valleys
if(length(vals)<10){
  vals<-c(1,vals)
  print(vals)
}

pkststeps<-timeSteps[pks]
valststeps<-timeSteps[vals]

pop1cycle<-popSize[c(2:12)]



### K curve

cycleTime<-10
totalCycle<-1
kMax<-30

#total runtime
runtime<-cycleTime*totalCycle
runtime

#all the time stpes
timeSteps<-c(0:runtime)
timeSteps

#cosine curve in a cycle time  
cosCurve<-cos(2*pi*timeSteps/cycleTime + pi)

#K curve
kCos<- round ((cosCurve+1)/2*kMax)


# final plot
#https://thepracticalr.wordpress.com/2016/08/30/2-y-axis-plotting/

par(mar = c(8,4,8,4))
plot(pop1cycle, type ="o", xlim=c(1,11),ylim=c(0,4900), ylab = "", xlab = "", lwd=2, xaxt = "n", yaxt = "n",
     col = "blue", bty="u")
title("Population dynamic", line=1)
axis(2,at=seq(0,4900,by=500),cex.axis=0.8)

par(new = TRUE)
plot(kCos, type = "o", xaxt = "n", yaxt = "n",xlim=c(1,11), ylim=c(0,34),
     ylab = "", xlab = "", col = "red", lty = 2, lwd=2, bty="u")

title(xlab="year", ylab="Population size", line=2, cex.lab=1)

axis(1,at=seq(0,10,by=1),cex.axis=0.8)


axis(side = 4, at=seq(0,34,by=5),cex.axis=0.8)
mtext("Carrying Capacity (K)", side = 4, line = 2)


legend("topleft", c("population", "K"), bty="n",
       col = c("blue", "red"), lty = c(1, 2),lwd=2)


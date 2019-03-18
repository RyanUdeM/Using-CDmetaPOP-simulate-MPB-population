
cycleTime<-10
totalCycle<-4
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

#
valSteps<- c(0:(totalCycle-1)*cycleTime) + 1
picSteps<- c(0:(totalCycle-1)*cycleTime) + ceiling(cycleTime*1/2)+1

kCos[picSteps]<-kMax
kCos[valSteps]<-0

plot(timeSteps,kCos, type="o",col="green4", lwd=3, ylim=c(0,35),xlab="",ylab="",xaxt="n", yaxt="n")
title("Different frequencies of Carrying Capacity over landscape", line = 1)

cycleTime<-20
totalCycle<-2
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

#
valSteps<- c(0:(totalCycle-1)*cycleTime) + 1
picSteps<- c(0:(totalCycle-1)*cycleTime) + ceiling(cycleTime*1/2)+1

kCos[picSteps]<-kMax
kCos[valSteps]<-0
par(new=TRUE)
plot(timeSteps,kCos, type="o",col="orange3", lwd=3, ylim=c(0,35),xlab="",ylab="",xaxt="n", yaxt="n")

cycleTime<-40
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

#
valSteps<- c(0:(totalCycle-1)*cycleTime) + 1
picSteps<- c(0:(totalCycle-1)*cycleTime) + ceiling(cycleTime*1/2)+1

kCos[picSteps]<-kMax
kCos[valSteps]<-0
par(new=TRUE)
plot(timeSteps,kCos, type="o",col="purple3", lwd=3, ylim=c(0,35),xlab="",ylab="",xaxt="n", yaxt="n")

title(xlab="year", ylab="Carrying Capacity (K)", line=2, cex.lab=1)

axis(1,  at=seq(0, 40,by=10),cex.axis=0.8)
axis(2,at=seq(0, 34,by=5),cex.axis=0.8)

legend("top",c("10-year","20-year","40_year"),lty=1,
       lwd=2,bty="n", 
       col=c("green4", "orange3","purple3"),
       ncol=3, cex = 1.25)

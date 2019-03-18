#####Fonction cree une courbe sinous de K#######


kCos<-function(cycleTime,totalCycle,kMax){
  
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
  
  plot(timeSteps,kCos, type="o",col="blue", lwd=1, main=paste("title"))
  
  return(kCos)
}


####test function####
kCos(40,10,60)
kCos(20,10,60)
kCos(10,10,60)




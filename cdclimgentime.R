cdclimgentime <- function(cycleTime, totalCycles) {
  #total runtime
  runtime <- cycleTime * totalCycles
  runtime
  
  #all the time stpes
  years <- c(0:runtime)
  years
  
  timeSteps<-years*2
  
  
  cdclimgentime<-paste(timeSteps,collapse="|") #because we use 2 timesteps for 1 year time. 
  write.table(cdclimgentime,file="cdclimgentime.csv",row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)
  return(cdclimgentime)
  
}
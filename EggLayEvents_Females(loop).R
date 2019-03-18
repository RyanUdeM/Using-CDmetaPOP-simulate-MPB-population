####reproduction through time#####


###Ryan summer 2018
###this script extract Females and EggLayEventslelic richness of population peaks and vEggLayEventsleys from CDmetaPOP simulation output

library(matrixStats)

directoryToSave<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_61x61_3x3_dis899_k30/heterozygosity"

path<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_61x61_3x3_dis899_k30/replicat"

senario<-"20y899"

Females_data <- paste("Females_", senario,".csv" ,sep = "")

EggLayEvents_data <- paste("EggLayEvents_", senario,".csv" ,sep = "")



Females.rep<-NULL

EggLayEvents.rep<-NULL


for(j in 0:9){
  
  directory<-paste(path,j,sep = "")
  setwd(directory)
  
  #############################################################
  #############################################################
  ####### part 1: import summary_popEggLayEventsltime.csv ###############
  #############################################################
  #############################################################
  
  
  #load simulation data, Femalesre we use summary_popEggLayEventslTime.csv
  summary_pop<-read.table("summary_popAllTime.csv",sep=",",header=TRUE,row.names = NULL)
  
  
  
  
  #extract timestep and population size
  popSummary<-summary_pop[,c(1,4)] #column 1 is timestep, column 4 is tFemales N_InitiEggLayEvents
  
  popu<-popSummary[-seq(2,nrow(popSummary),2),,drop=F] #use only tFemales data of tFemales even number timesteps
  
  write.table(popu[,2],file="indivs.csv",sep=",",quote=F,row.names=F,col.names=F)
  pop<-read.table("indivs.csv",sep="|")
  popSize<-NULL #vector that has tFemales population size of All timesteps
  for(i in 1:nrow(popu)){
    popSize<-c(popSize,pop[i,1])
  }
  
  plot(popSize,type="o", main="popSize")
  
  #chosen timesteps (even timesteps)
  write.table(popu[,1],file="time.csv",sep=",",quote=F,row.names=F,col.names=F)
  time<-read.table("time.csv")
  timeSteps<-NULL #vector that has tFemales population size of All timesteps
  for(i in 1:nrow(popu)){
    timeSteps<-c(timeSteps,time[i,1])
  }
  
  
  #extract Females and EggLayEventslelic Richness
  EggLayEvents_Females<-summary_pop[,c(13,9)] #col13 is EggLayEvents, col9 is Females
  EggLayEvents_Females<-EggLayEvents_Females[-seq(1,(nrow(EggLayEvents_Females)-1),2),,drop=F] #keep only the even time steps(begining of each generation)
  
  #Females
  write.table(EggLayEvents_Females[,2],file="Females.csv",sep=",",quote=F,row.names=F,col.names=F)
  Females0<-read.table("Females.csv",sep="|")
  Females<-NULL 
  for(i in 1:nrow(Females0)){
    Females<-c(Females,Females0[i,1])
  }
  Females.rep<-rbind(Females.rep,Females)
  plot(Females,type="o",main="Females")
  
  #EggLayEventslelic Richness
  write.table(EggLayEvents_Females[,1],file="EggLayEvents.csv",sep=",",quote=F,row.names=F,col.names=F)
  EggLayEvents0<-read.table("EggLayEvents.csv",sep="|")
  EggLayEvents<-NULL #vector that has tFemales population size of EggLayEventsl timesteps
  for(i in 1:nrow(EggLayEvents0)){
    EggLayEvents<-c(EggLayEvents,EggLayEvents0[i,1])
  }
  EggLayEvents.rep<-rbind(EggLayEvents.rep,EggLayEvents)
  plot(EggLayEvents,type="o",main="EggLayEventslelic richness")
}



#save Females data
setwd(directoryToSave)
write.csv(Females.rep,file=Females_data)
write.csv(EggLayEvents.rep,file=EggLayEvents_data)

###mean
Females_mean<-colMeans(Females.rep)
EggLayEvents_mean<-colMeans(EggLayEvents.rep)

reproducRate<-EggLayEvents_mean/Females_mean



#save sd to RData
nam<-paste("EggLayRate","_",senario,sep="")
assign(nam, reproducRate)
rat<-paste(nam,".RData",sep="")
save(list=nam,file = rat)



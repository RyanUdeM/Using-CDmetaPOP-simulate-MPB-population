####heterozygosity through time#####


###Ryan summer 2018
###this script extract He and Allelic richness of population peaks and valleys from CDmetaPOP simulation output

library(matrixStats)

directoryToSave<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/heterozygosity"

path<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/replicat"

senario<-"40y899"

He_data <- paste("He_", senario,".csv" ,sep = "")

Al_data <- paste("Al_", senario,".csv" ,sep = "")



  He.rep<-NULL
 
  Al.rep<-NULL

  
  for(j in 0:9){
    
    directory<-paste(path,j,sep = "")
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
    
    plot(popSize,type="o", main="popSize")
    
    #chosen timesteps (even timesteps)
    write.table(popu[,1],file="time.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
    time<-read.table("time.csv")
    timeSteps<-NULL #vector that has the population size of all timesteps
    for(i in 1:nrow(popu)){
      timeSteps<-c(timeSteps,time[i,1])
    }
    
    
    #extract He and Allelic Richness
    AlRich_He<-summary_pop[,c(32,33)] #col32 is Allelic richness, col33 is He
    AlRich_He<-AlRich_He[-seq(2,nrow(AlRich_He),2),,drop=FALSE] #keep only the even time steps(begining of each generation)
    
    #He
    write.table(AlRich_He[,2],file="He.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
    Hetero<-read.table("He.csv",sep="|")
    He<-NULL 
    for(i in 1:nrow(Hetero)){
      He<-c(He,Hetero[i,1])
    }
    He.rep<-rbind(He.rep,He)
    plot(He,type="o",main="He")
    
    #Allelic Richness
    write.table(AlRich_He[,1],file="Al.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
    AlRichness<-read.table("Al.csv",sep="|")
    Al<-NULL #vector that has the population size of all timesteps
    for(i in 1:nrow(AlRichness)){
      Al<-c(Al,AlRichness[i,1])
    }
    Al.rep<-rbind(Al.rep,Al)
    plot(Al,type="o",main="allelic richness")
  }
  
  

#save He data
  setwd(directoryToSave)
  write.csv(He.rep,file=He_data)
  write.csv(Al.rep,file=Al_data)
  
###mean
  He_mean<-colMeans(He.rep)
  Al_mean<-colMeans(Al.rep)
  
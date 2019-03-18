###Ryan summer 2018
###this script extract He and Allelic richness of population peaks and valleys from CDmetaPOP simulation output
accu<-2

senario<-"10y_199"

He<-NULL

direc<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30"

#change the directory to the output of jth replicat 
for(j in 0:9){
directory<-paste(direc,"/replicat",j,sep = "")
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

plot(He,type="o",main="He")

#Allelic Richness
write.table(AlRich_He[,1],file="Al.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
AlRichness<-read.table("Al.csv",sep="|")
Al<-NULL #vector that has the population size of all timesteps
for(i in 1:nrow(AlRichness)){
  Al<-c(Al,AlRichness[i,1])
}
plot(Al,type="o",main="allelic richness")

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


#He of pks and vals
He_pks<-He[pks]
He_vals<-He[vals]

He<-rbind(He,He_pks)

#Al of pks ans vals
Al_pks<-Al[pks]
Al_vals<-Al[vals]
}

#save He at pks to RData
setwd(direc)
filename<-paste("He_",senario,sep="")
assign(filename, He)
file<-paste(filename,".RData",sep="")
save(list=filename,file = file)

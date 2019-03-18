###Ryan 2018 summer
###script import summary_popAlltime.csv (CDmetaPOP simulation output)
###find peak timesteps and valley timesteps accroding to initial population size of timestep

#install.packages("devtools")
#install.packages("adegenet")
#install.packages("pcadapt")
#install.packages("pegas")
#install.packages("sp")
#install.packages("hierfstat")

library(devtools);
library(adegenet);
library(pcadapt);
library(pegas);
library(sp);
library(hierfstat);

### a 'peak' is defined as a local maxima with "accu" points either side of it being smaller than it. 
accu<-10  #10year senario use 2; 20year use 5; 40year use 10;
nbr.loc<-100
#decide regular sampling plan: sample size of peak populations, % of valley populations
pkSample<-200 #this is the sample size
valSample<-10 #this is percentage 10 means 10%


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30/replicat0"
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

plot(popSize,type="o")

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


##################################################################################
####*****fonction that returns file names of the chosen peaks and valleys****#####
##################################################################################
find_files <- function(x){
  t<-NULL
  for (i in 1:length(x))
    t <- paste("ind",x,".csv",sep = "")
  return(t)
}
####*****end of the fonction****####

##return the file names of the peaks and valleys

pksfiles<-find_files(pkststeps) ##peaks to be imported and converted into genind for future tests
valsfiles<-find_files((valststeps))  ##valleys to be imported and converted into genind for future tests

print(paste("the last 10 peaks are:"))
print(timeSteps[pks])
print(paste("the last 10 valleys are:"))
print(timeSteps[vals])


################################2018_06-08######################################################

#############################################################
#############################################################
### part 3: export matadatvals.csv and metadatpks.csv  ######
#############################################################
#############################################################

###metadata pks (sampling = 200 individuls from each peak)#########
metadatpks<-data.frame()
for (i in 1:length(pkststeps)){
  GRID<-read.csv(pksfiles[i], sep = ",",header = TRUE)
  
  #pkSample is the sample size of peak populations
  #%%%%%%%%   regular sample from indx.csv, interval =     %%%%%%%%%%%%%%%%
  interval<- popSize[pks[i]]/200
  
  #extract 10% from the indx.csv file of peak timesteps
  samples<-round(seq(1,nrow(GRID),interval))
  GRID<-GRID[samples,]
  
  individu<-cbind(pkststeps[i],GRID)
  metadatpks<-rbind(metadatpks,individu)

}

write.table(metadatpks,file="metadatpks.csv",row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)

#######metadata valleys (sampling = 10% of the population)#######
metadatvals<-data.frame()
for (i in 1:length(valststeps)){
  GRID<-read.csv(valsfiles[i], sep = ",",header = TRUE)
    
  #%%%%%%%%   regular sample from indx.csv, interval = 10    %%%%%%%%%%%%%%%%%
  #extract 10% from the indx.csv file of valley timesteps
    samples<-seq(1,nrow(GRID),10)
    GRID<-GRID[samples,]
    
    individu<-cbind(valststeps[i],GRID)
    metadatvals<-rbind(metadatvals,individu)
}

write.table(metadatvals,file="metadatvals.csv",row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)



#######################################################################
#######################################################################
### part 4: convert matadatvals.csv and metadatpks.csv to genind ######
#######################################################################
#######################################################################

#######*******************************************************************#####
#######********function that import metadatapks.csv to genind*************#####
#######*******************************************************************#####

importind<-function(directory,dataframe,nbr.loc,time){
  setwd(directory);
  GRID<-read.csv(dataframe, sep = ",",header = TRUE);
  GRID[1:5,1:10]
  dim(GRID)
  
  #add time information for the indx.csv
  ##GRID$Subpopulation <- interaction( time, GRID$Subpopulation, sep = "_pop")
  
  #For unfilled grids
  GRID<-GRID[complete.cases(GRID),]
  dim(GRID)
  
  ## Extract coordinates
  xy.coord<-GRID[,3:4];
  head(xy.coord);
  str(xy.coord);
  xy.coord.cell<-xy.coord[!duplicated(xy.coord[1:2]),];
  
  ##################### LFMM #################################################
  
  #Split the genotypic and non-genotypic columns
  GRID_info<-GRID[,1:17];
  GRID_gen<-GRID[,-c(1:17)];
  
  #Having two columns per locus is useless as we know the sum of both per individual is 1
  #Take those off
  GRID_gen<-GRID_gen[,-seq(2,dim(GRID_gen)[2],2),drop=FALSE];
  
  XYXY<-interaction(xy.coord[,1], xy.coord[,2], sep=  "_", lex.order = FALSE)
  mat.cell<-rowsum(as.matrix(GRID_gen), XYXY);
  mat.cell[,1:2];
  head(mat.cell)
  dim(mat.cell)
  
  for (i in 1:ncol(GRID_gen)){
    for (j in 1:nrow(GRID_gen)){
      if(GRID_gen[j,i]=="0"){
        GRID_gen[j,i]="a,a"
      }else if (GRID_gen[j,i]=="1"){
        GRID_gen[j,i]="A,a"
      }else if (GRID_gen[j,i]=="2"){
        GRID_gen[j,i]="A,A"
      }
    }
  }
  
  #names of individus
  ind.names<-GRID[,5]
  
  #soupopulation of individus
  pop<-GRID[,1]
  
  #names of the loci
  loci.names=NULL
  for (i in 1:nbr.loc){
    loci.names=c(loci.names,i-1)
    loci.names[i-1]=paste("L",loci.names[i-1],sep="")
  }
  
  #convert GRID_gen to genind
  genind<-df2genind(GRID_gen,ncode=2,sep=",",pop=pop,ind.names=ind.names,loc.names=loci.names)
  
  #create a vector of the timestep
  timestep<-rep(time,nrow(genind@tab))
  timestep<-data.frame(timestep)
  #add timestep information in the strata of the genind
  #genind<-strata(genind, formula = NULL, combine = TRUE, timestep)
  
  return(genind)
}

#######****************end of the function********###############
#######*******************************************###############



#######################################################################
#######################################################################
### part 5: calculate pirewise Fst of valleys and peaks          ######
#######################################################################
#######################################################################

#fst of valleys
genindvals<-importind(directory,"metadatvals.csv",nbr.loc,time)

namGenind <- paste("genindvals", i, sep = "")
assign(namGenind, genindvals)
#save(list=namGenind,file = fileGenind)

namFstP <- paste("valsFstP", i, sep = "")
valsFstP<-pairwise.fst(genindvals, pop = NULL, res.type = c("dist", "matrix"))
assign(namFstP, valsFstP)
#save(list=namFstP,file = fileFstP)


#fst of peaks
genindpks<-importind(directory,"metadatpks.csv",nbr.loc,time)

namGenind <- paste("genindpks", i, sep = "")
assign(namGenind, genindpks)
#save(list=namGenind,file = fileGenind)

namFstP <- paste("pksFstP", i, sep = "")
pksFstP<-pairwise.fst(genindpks, pop = NULL, res.type = c("dist", "matrix"))
assign(namFstP, pksFstP)
#save(list=namFstP,file = fileFstP)


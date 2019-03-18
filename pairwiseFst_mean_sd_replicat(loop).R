#ce code tranfert les metaData des pks et vals en genind
#puis calculer le FstPairwise
#sauvgarder les geninds et FstP sous forme de Rdata

#install.packages("adegenet")
library(adegenet)
library(hierfstat)

directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_51x51_3x3_dis299_k30/metadat"

FstP.list<-NULL


for (i in 0:9){

###  change these parameters to save Rdata with according to data
### It  changes the object name before save to Rdata

  
class<-"pks"   #vals or pks, (this script can be used for both pks and vals)
senario<-"40y299"



time<- paste("40y_",i,sep ="") #not used

dataframe <- paste("metadat", class, i,".csv" ,sep = "")

namFstP <- paste(class,"FstP_",senario,"_", i, sep = "")

namGenind <- paste("genind",class,"_",senario,"_", i, sep = "")

nbr.loc <- 100


#############################################################################
#######********function that import metadatapks.csv to genind*************###########  
#############################################################################
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
  genind<-strata(genind, formula = NULL, combine = TRUE, timestep)
  
  return(genind)
}

#######****************end of the function********###############

genindpks<-importind(directory,dataframe,nbr.loc,time)

##d <- 5
##for(i in 1:10) { 
#     nam <- paste("A", i, sep = "")
#     assign(nam, rnorm(3)+d)
# }
#donne des variables Ai selon le for boucle

#save genind file of each pk
assign(namGenind, genindpks)
fileGenind<- paste(namGenind,".RData",sep = "")
save(list=namGenind,file = fileGenind)

#save pairwise fst file of each pk
pksFstP<-pairwise.fst(genindpks, pop = NULL, res.type = c("dist", "matrix"))
assign(namFstP, pksFstP)
fileFstP <- paste(namFstP,".RData", sep = "")
save(list=namFstP,file = fileFstP)

FstP.list[[i+1]]<-as.matrix(pksFstP)

}

#calculate mean and sd
#https://stackoverflow.com/questions/19218475/element-wise-mean-over-list-of-matrices

mean_FstP<-apply(simplify2array(FstP.list), 1:2, mean)
sd_FstP<-apply(simplify2array(FstP.list), 1:2, sd)

#save mean to RData
Mean<-paste("FstP_Mean_",class,"_",senario,sep="")
assign(Mean, mean_FstP)
fileMean<-paste(Mean,".RData",sep="")
save(list=Mean,file = fileMean)

#save sd to RData
Sd<-paste("FstP_Sd_",class,"_",senario,sep="")
assign(Sd, sd_FstP)
fileSd<-paste(Sd,".RData",sep="")
save(list=Sd,file = fileSd)


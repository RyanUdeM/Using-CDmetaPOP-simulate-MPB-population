####This script is written by Ryan for his 2017 summer internship in Labjames#####
####this script takes the will: 
####1. find the center refuge of a nxn grid, and show's a regular sampling plan
####2. goes through the output, find the population peaks and valleys (individual numbers)
####3. turns peaks and valleys into Genind object
####4. save Genind object to Rdata


library(devtools);
library(adegenet);
library(pcadapt);
library(pegas);
library(sp);
library("hierfstat");

#choose directory of the dataframe (CDmetaPOP)
directory<-"D:/Stage Lab James 2017 Summer/Ryan summer 2017/ppt slides for Patrick/Fst caculate abstract/batchrun0mcrun0"
setwd(directory)

#number of loci simulated in the senario
nbr.loc<-100

col<-17
row<-17
###centre refuge size###
xcen<-3
ycen<-3

#############################################################
#############################################################
######## part 1 sampling % of the sites######################
#############################################################
library(RANN)
#library(rgdal)
#define grid size
col<-col
row<-row

#create the dataframe of all the sites with their coordinate
XCOORDm<-rep(1:col,times=row)
YCOORDm<-rep(1:row,each=col)
COORDm<-as.data.frame(cbind(XCOORDm,YCOORDm))

#create the grid
grid<-t(matrix((1:(col*row)),ncol=col))

#find the centre of the grid 
if (col%%2==0){
  xcent<-c(col/2,col/2+1)
}else{
  xcent<-(col+1)/2
}

if (row%%2==0){
  ycent<-c(row/2,row/2+1)
}else{
  ycent<-(row+1)/2
}

gridcent<-NULL
for (i in 1:length(xcent)){
  for (j in 1:length(ycent)){
    gridcent<-c(gridcent,grid[xcent[i],ycent[j]])
  }
}


#####**********function find grid centre***********####
#need col and row number, dimention of the centre wanted
centre<-function(col,row,xcen,ycen){
  xcentre<-(col/2-xcen/2+1):(col/2+xcen/2)
  ycentre<-(row/2-ycen/2+1):(row/2+ycen/2)
  centre<-NULL
  for (i in 1:length(xcentre)){
    for (j in 1:length(ycentre)){
      
      xy<-((xcentre[i]-1)*col+ycentre[j])
      centre<-c(centre,xy)
    }
  }
  return(centre)
}
####*********end of the function**********#########
### define refuge cells if dont want random refuges
###refuge size###
xcen<-xcen
ycen<-ycen
refuges<-centre(col,row,xcen,ycen)
print(paste("refuges sites ="))
print(refuges)

#regular sampling sites
###***this part below works only for grid with impair rows and columns***###
skip<-2 #how many cases have to be skipped when sampling from the centre
regsamp<-function(cent, max, skip){
  samp1<-NULL
  s1<-cent
  while ((s1-(skip+1))>1) {
    s1<-s1-(skip+1)
    samp1<-c(samp1,s1)
  }
  
  samp2<-NULL
  s2<-cent
  while((s2+(skip+1))<=(max-1)){
    s2<-s2+(skip+1)
    samp2<-c(samp2,s2)
  }
  samp<-sort(c(samp1,xcent,samp2))
}

xsamp<-regsamp(xcent,col,skip)
ysamp<-regsamp(ycent,row,skip)
samples<-grid[xsamp,ysamp]

#return sites chosen into list
sites<-NULL
for (i in 1:length(samples)){
  sites<-c(sites,samples[i])
}

print(paste("Number of sampling sites =",length(sites),";",sep=" "))
print(paste("percentage if sampling =",round(length(sites)/((row-2)*(col-2))*10000)/100,"%",";",sep=" "))
print(paste("samoling sites are:"))
print(sites)

#plot chosen sites
color <- rep(1, nrow(COORDm))
color[sites] <- 2
COORDm <- cbind.data.frame(COORDm, color=color)

COORDm2 <- COORDm
coordinates(COORDm2) <- ~XCOORDm+YCOORDm
gridded(COORDm2) <- TRUE
plot(COORDm2,col=c("white","red"),border="Black")


##############################################################
##############################################################
#### part 2: find peaks and valleys and convert###############
##############################################################
##############################################################


#create a list of all the indx.csv after simulation
indlist<-list.files(directory,pattern="?.csv")

#return the population size for every timestep file.
totalind<-numeric(length(indlist))
timestep<-numeric(length(indlist))

for (i in 1:length(indlist)){
  totalind[i]<-nrow(read.csv(indlist[[i]],header=TRUE))
  timestep[i]<-gsub("(*[[:lower:]]*)(*[[:digit:]]*)([[:punct:]])(*[[:lower:]]*)", "\\2", indlist[[i]])
}

#create a dataframe with totals and corresponding timesteps
df<-cbind(as.integer(totalind),as.integer(timestep))
colnames(df)<-c("TotalInd","Timestep")
#To sort the dataframe by timestep, we create an index of how to sort the rows of df so that they are ascending
index<-order(df[,2])
df<-df[index,]
#delete NA rows
df<-df[complete.cases(df), ]

#########################################
####*****find peaks fonction***##########
###notice:a 'peak' is defined as a local maxima with m points either side of it being smaller than it. 
###hence, the bigger the parameter m, the more stringent is the peak funding procedure
find_peaks <- function (x, m = 3){
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
find_valleys <- function (x,m = 3){
  vals<-find_peaks(-x,m)
  vals
}

pks<-find_peaks(df[,1])
vals<-find_valleys(df[,1])

#function that delete fake pks (which cause by 2 continuous kmax or k=0 in the k string)
#remove pk if it is in the m steps following the previous peak.
truepks<-function(pks,m){
  fakepks<-NULL
  for (i in 2:length(pks)){
    ss<-pks[i]-pks[i-1]
    if (ss<m) {
      fakepks<-c(fakepks,i)
    }
  }
  if(length(fakepks)>0){
    pks<-pks[-fakepks]
    pks
  }else{
    pks
  }
  pks <-pks[(length(pks)-10):(length(pks)-1)]
}

pks<-truepks(pks,3)
vals<-truepks(vals,3)

pkststeps<-df[,2][pks]
valststeps<-df[,2][vals]


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
print(df[pks,])
print(paste("the last 10 valleys are:"))
print(df[vals,])


#############################################################################
#######********function that import ind.csv to genind*************###########  
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
  xy.coord<-GRID[,2:3];
  head(xy.coord);
  str(xy.coord);
  xy.coord.cell<-xy.coord[!duplicated(xy.coord[1:2]),];
  
  ##################### LFMM #################################################
  
  #Split the genotypic and non-genotypic columns
  GRID_info<-GRID[,1:16];
  GRID_gen<-GRID[,-c(1:16)];
  
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
  ind.names<-GRID[,4]
  
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

###import & convert valleys
valsinds<-list()
for (i in 1:length(vals)){
  genindval<-importind(directory,dataframe=valsfiles[i],nbr.loc,time=valststeps[i])
  ###choose only the pop in the refuges
  valsinds[i]<-genindval[genindval@pop %in% refuges]
}
print(paste("genind file of valleys ="))
print(valsinds)
###merge all valleys genind files together into one genind
indvalley<-repool(valsinds, list = FALSE)
indvalley
save(indvalley, file="indOfAllValleys.RData")

###import & convert peaks
pksinds<-list()
for (i in 1:length(pks)){
  genindpk<-importind(directory,dataframe=pksfiles[i],nbr.loc,time=pkststeps[i])
  ###choose only the pop in the sampling sites
  pksinds[i]<-genindpk[genindpk@pop %in% sites]
  
}
print(paste("genind file of peaks ="))
print(pksinds)
###merge all peaks genind files together into one genind
indpeak<-repool(pksinds, list = FALSE)
indpeak
save(indpeak, file="indOfAllPeaks.RData") #use load("indOfAllPeaks.RDate") to restore genind pksinds

###calculate Pairwised Fst
valsFstP<-pairwise.fst(indvalley, pop = NULL, res.type = c("dist", "matrix"))
valsFstP
save(valsFstP, file="valsFstP.RData")

pksFstP<-pairwise.fst(indpeak, pop = NULL, res.type = c("dist", "matrix"))
pksFstP
save(pksFstP, file="pksFstP.RData")





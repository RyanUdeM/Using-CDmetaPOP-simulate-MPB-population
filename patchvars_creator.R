##### Function create Patchvars.csv #######

setwd("C:/Users/Guest/Desktop/Ryan/2018 summer")


titre<-"Patchvars.csv"
#define grid size
col<-41
row<-41
#total number of cycles
totalCycles<-40
#outbreak cycle time (years)
cycleTime<- 10
#Max carring capacity
kMax<-30
#kBurnIn<-kMax
#N0.BurnIn<-60
#time.BurnIn<-0 #timesteps for burn-in

#create K for patchvars
source("K_cosinusoidal.R")
k<-kCos(cycleTime,totalCycles,kMax)
#k<-c(kBurnIn,k)
k_final<-paste(k,collapse="|")


#decide refuges
#ex centre refuges 
xcen<-3
ycen<-3
source("refuge_central.R")
refuge_centre <- centre(col, row, xcen, ycen)
refuge.cells<-refuge_centre
refuge.cells

#decide Burn in area
#source("refuge_central.R")
#burnin.x<-11
#burnin.y<-11
#burnin.cells <- centre(col, row, burnin.x, burnin.y)
#burnin.cells

#Create fichier Patchvars
X<-rep(1:col,times=row)
Y<-rep(1:row,each=col)
Patch<-c(1:(col*row))
K<-k_final
K_StDev<-0
N0<-0    
Natal_Grounds<-1
Migration_Grounds<-1
Genes_Initialize<-"random"
Class_Vars<-"ClassVars.csv"
Mortality_Out<-"N"
Mortality_Out_StDev<-0
Mortality_Back<-"N"
Mortality_Back_StDev<-0
Mortality_Eggs<-0
Mortality_Eggs_StDev<-0
Migration<-0
Migration_Set<-"N"
Migration_Straying<-1
GrowthTemperatureOut<-"N"
GrowthTemperatureOutStDev<-0.2
GrowDaysOut<-150
GrowDaysOutStDev<-20
GrowthTemperatureBack<-"N"
GrowthTemperatureBackStDev<-0
GrowDaysBack<-150
GrowDaysBackStDev<-20
Capture_Probability_Out<-"N" 
Capture_Probability_Back<-"N"
Fitness_AA<-0
Fitness_Aa<-0
Fitness_aa<-0
Fitness_BB<-0
Fitness_Bb<-0
Fitness_bb<-0
Fitness_AABB<-0
Fitness_AaBB<-0
Fitness_aaBB<-0
Fitness_AABb<-0
Fitness_AaBb<-0
Fitness_aaBb<-0
Fitness_AAbb<-0
Fitness_Aabb<-0
Fitness_aabb<-0

patchvars<-cbind(Patch,X,Y,K,K_StDev,N0,Natal_Grounds,Migration_Grounds,Genes_Initialize,Class_Vars,Mortality_Out,Mortality_Out_StDev,Mortality_Back,Mortality_Back_StDev,Mortality_Eggs,Mortality_Eggs_StDev,Migration,Migration_Set,Migration_Straying,GrowthTemperatureOut,GrowthTemperatureOutStDev,GrowDaysOut,GrowDaysOutStDev,GrowthTemperatureBack,GrowthTemperatureBackStDev,GrowDaysBack,GrowDaysBackStDev,Capture_Probability_Out,Capture_Probability_Back,Fitness_AA,Fitness_Aa,Fitness_aa,Fitness_BB,Fitness_Bb,Fitness_bb,Fitness_AABB,Fitness_AaBB,Fitness_aaBB,Fitness_AABb,Fitness_AaBb,Fitness_aaBb,Fitness_AAbb,Fitness_Aabb,Fitness_aabb)


#Extraction variables en bordure de la grille
death.cells<-which(patchvars[,2]==1|patchvars[,2]==row|patchvars[,3]==1|patchvars[,3]==col) # X=1 and X=row ; Y=1 and Y=col
for(i in death.cells){
  #On met une mortalit? de 100 aux cases en bordure
  patchvars[i,13]=100
  #On fait en sorte de ne commencer avec aucun individu dans les cases de bordure
  patchvars[i,6]=0
}

#change variables of refuge pixels
for(i in refuge.cells){
  #refuge keeps K = kMax
  patchvars[i,4]=kMax
  #N0 in the refuge = kMax
  patchvars[i,6]=kMax
}


#change variables of burn in pixels
#for(i in burnin.cells){
#  patchvars[i,6]=kMax #N0 = kMax in refuge cells
#}

##creat BurnInMatrix
#BurnInMatrix<-matrix(99,col*row,col*row)
#for(i in burnin.cells ){
#  for(j in burnin.cells){
#    BurnInMatrix[i,j]<-0
#  }
#}
#write.table(BurnInMatrix,file="BurnInMatrix.csv",row.names=FALSE,col.names=FALSE,sep=",")



####****************Create Patchvars.csv*******######
write.table(patchvars,file=titre,row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)

#create cdclimgentime for PopVars
source("cdclimgentime.R")
cdclimgentime<- cdclimgentime(cycleTime, totalCycles)


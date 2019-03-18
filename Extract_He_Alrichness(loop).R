###Ryan summer 2018
###this script extract He and Allelic richness of population peaks and valleys from CDmetaPOP simulation output

library(matrixStats)

accu<-10

path<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/replicat"


senario_hetero<-function(path){
#create variable to save He and Allrichness for secario
He_pks.rep<-NULL
He_vals.rep<-NULL
Al_pks.rep<-NULL
Al_vals.rep<-NULL

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
  #print(vals)
}

pkststeps<-timeSteps[pks]
valststeps<-timeSteps[vals]


#He of pks and vals
He_pks<-He[pks]

print(He_pks)

He_pks.rep<-rbind(He_pks.rep,He_pks)

print(He_pks.rep)

He_vals<-He[vals]
He_vals.rep<-rbind(He_vals.rep,He_vals)

#Al of pks ans vals
Al_pks<-Al[pks]
Al_pks.rep<-rbind(Al_pks.rep,Al_pks)
Al_vals<-Al[vals]
Al_vals.rep<-rbind(Al_vals.rep,Al_vals)

}

return(He_pks.rep)
}

#dispersal 199
accu<-2
path_10y_199<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30/replicat"
He_10y_199<-senario_hetero(path_10y_199)

accu<-10
path_20y_199<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_21x21_3x3_dis199_k30/replicat"
He_20y_199<-senario_hetero(path_20y_199)
   
accu<-20
path_40y_199<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_41x41_3x3_dis199_k30/replicat"
He_40y_199<-senario_hetero(path_40y_199)

#dispersal 299
accu<-2
path_10y_299<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis299_k30/replicat"
He_10y_299<-senario_hetero(path_10y_299)

accu<-10
path_20y_299<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_31x31_3x3_dis299_k30/replicat"
He_20y_299<-senario_hetero(path_20y_299)

accu<-20
path_40y_299<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_51x51_3x3_dis299_k30/replicat"
He_40y_299<-senario_hetero(path_40y_299)

#dispersal 899
accu<-2
path_10y_899<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_41x41_3x3_dis899_k30/replicat"
He_10y_899<-senario_hetero(path_10y_899)

accu<-10
path_20y_899<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_61x61_3x3_dis899_k30/replicat"
He_20y_899<-senario_hetero(path_20y_899)

accu<-20
path_40y_899<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/replicat"
He_40y_899<-senario_hetero(path_40y_899)







#########################################################
############################# figure of Heterozygocity ####################
#########################################################
par(mar=c(3,5,3,5))
#######DISPERDAL = 199 ###################
#####calculate mean and sd

He_10y_199_mean<-colMeans(He_10y_199)
He_10y_199_sd<-colSds(He_10y_199)

foo_10y_199 <- data.frame(peak=c(1:10),
                          mean=He_10y_199_mean,
                          sd=He_10y_199_sd)

plot(1:10,foo_10y_199$mean,pch=19,main="Loss of genetic variation",cex.main=2, type="o" ,col="green4", xlab="",ylab="",xaxt="n",
     lwd=3,bty='l',
     ylim=c(0,max((foo_10y_199$mean+foo_10y_199$sd))),cex.axis=1.5)

arrows(c(1:10),foo_10y_199$mean-foo_10y_199$sd,c(1:10),foo_10y_199$mean+foo_10y_199$sd,code=3,length=0.02,angle=90,col='green4')
title(xlab="outbreak cycle", ylab="Expected heterozygosity (He) at peak ", line=2, cex.lab=1.3)

#lines(rbind(1:10,1:10,NA),rbind(foo_10y_199$mean-foo_10y_199$sd,foo_10y_199$mean+foo_10y_199$sd,NA))

par(new=TRUE)

He_20y_199_mean<-colMeans(He_20y_199)
He_20y_199_sd<-colSds(He_20y_199)

foo_20_199 <- data.frame(peak=c(1:10),
                         mean=He_20y_199_mean,
                         sd=He_20y_199_sd)

plot(1:10,foo_20_199$mean,pch=19,type="o" ,col="orange3",xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=3,bty='l',
     ylim=c(0,max((foo_20_199$mean+foo_20_199$sd))))
axis(side=1, at=c(0:10))

arrows(c(1:10),foo_20_199$mean-foo_20_199$sd,c(1:10),foo_20_199$mean+foo_20_199$sd,code=3,length=0.02,angle=90,col='orange3')

#lines(rbind(1:10,1:10,NA),rbind(foo_20_199$mean-foo_20_199$sd,foo_20_199$mean+foo_20_199$sd,NA),col="orange3")

par(new=TRUE)

He_40y_199_mean<-colMeans(He_40y_199)
He_40y_199_sd<-colSds(He_40y_199)

foo_40_199 <- data.frame(peak=c(1:10),
                         mean=He_40y_199_mean,
                         sd=He_40y_199_sd)

plot(1:10,foo_40_199$mean,pch=19,type="o" ,col="purple3",xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=3,bty='l',
     ylim=c(0,max((foo_40_199$mean+foo_40_199$sd))))

arrows(c(1:10),foo_40_199$mean-foo_40_199$sd,c(1:10),foo_40_199$mean+foo_40_199$sd,code=3,length=0.02,angle=90,col="purple3")


#lines(rbind(1:10,1:10,NA),rbind(foo_40_199$mean-foo_40_199$sd,foo_40_199$mean+foo_40_199$sd,NA),col="grey")

#legend("bottomleft", legend=c("10-year cycle", "20-year cycle","40-year cycle"),
 #      col=c("blue","orange3", "purple3"), lty=1, cex=1)




#######DISPERDAL = 299 ###################
#####calculate mean and sd

par(new=TRUE)


He_10y_299_mean<-colMeans(He_10y_299)
He_10y_299_sd<-colSds(He_10y_299)

foo_10y_299 <- data.frame(peak=c(1:10),
                          mean=He_10y_299_mean,
                          sd=He_10y_299_sd)

plot(1:10,foo_10y_299$mean,pch=19, type="o" ,col="green4", xlab="",ylab="",xaxt="n", yaxt="n",lty=5,lwd=3,bty='l',
     ylim=c(0,max((foo_10y_299$mean+foo_10y_299$sd))))

arrows(c(1:10),foo_10y_299$mean-foo_10y_299$sd,c(1:10),foo_10y_299$mean+foo_10y_299$sd,code=3,length=0.02,angle=90,col='green4')


#lines(rbind(1:10,1:10,NA),rbind(foo_10y_299$mean-foo_10y_299$sd,foo_10y_299$mean+foo_10y_299$sd,NA))

par(new=TRUE)

He_20y_299_mean<-colMeans(He_20y_299)
He_20y_299_sd<-colSds(He_20y_299)

foo_20_299 <- data.frame(peak=c(1:10),
                         mean=He_20y_299_mean,
                         sd=He_20y_299_sd)

plot(1:10,foo_20_299$mean,pch=19,type="o" ,col="orange3",xlab="",ylab="",xaxt="n", yaxt="n", lty=5,lwd=3,bty='l',
     ylim=c(0,max((foo_20_299$mean+foo_20_299$sd))))
axis(side=1, at=c(0:10))

arrows(c(1:10),foo_20_299$mean-foo_20_299$sd,c(1:10),foo_20_299$mean+foo_20_299$sd,code=3,length=0.02,angle=90,col='orange3')


#lines(rbind(1:10,1:10,NA),rbind(foo_20_299$mean-foo_20_299$sd,foo_20_299$mean+foo_20_299$sd,NA),col="orange3")

par(new=TRUE)

He_40y_299_mean<-colMeans(He_40y_299)
He_40y_299_sd<-colSds(He_40y_299)

foo_40_299 <- data.frame(peak=c(1:10),
                         mean=He_40y_299_mean,
                         sd=He_40y_299_sd)

plot(1:10,foo_40_299$mean,pch=19,type="o" ,col="purple3",xlab="",ylab="",xaxt="n", yaxt="n", lty=5,lwd=3,bty='l',
     ylim=c(0,max((foo_40_299$mean+foo_40_299$sd))))

arrows(c(1:10),foo_40_299$mean-foo_40_299$sd,c(1:10),foo_40_299$mean+foo_40_299$sd,code=3,length=0.02,angle=90,col="purple3")


#lines(rbind(1:10,1:10,NA),rbind(foo_40_299$mean-foo_40_299$sd,foo_40_299$mean+foo_40_299$sd,NA),col="grey")


#######DISPERDAL = 899 ###################
#####calculate mean and sd

par(new=TRUE)

He_10y_899_mean<-colMeans(He_10y_899)
He_10y_899_sd<-colSds(He_10y_899)

foo_10y_899 <- data.frame(peak=c(1:10),
                          mean=He_10y_899_mean,
                          sd=He_10y_899_sd)

plot(1:10,foo_10y_899$mean,pch=19, type="o" ,col="green4", xlab="",ylab="",xaxt="n", yaxt="n", lty=3,lwd=3,bty='l',
     ylim=c(0,max((foo_10y_899$mean+foo_10y_899$sd))))

arrows(c(1:10),foo_10y_899$mean-foo_10y_899$sd,c(1:10),foo_10y_899$mean+foo_10y_899$sd,code=3,length=0.02,angle=90,col='green4')


#lines(rbind(1:10,1:10,NA),rbind(foo_10y_899$mean-foo_10y_899$sd,foo_10y_899$mean+foo_10y_899$sd,NA))

par(new=TRUE)

He_20y_899_mean<-colMeans(He_20y_899)
He_20y_899_sd<-colSds(He_20y_899)

foo_20_899 <- data.frame(peak=c(1:10),
                         mean=He_20y_899_mean,
                         sd=He_20y_899_sd)

plot(1:10,foo_20_899$mean,pch=19,type="o" ,col="orange3",xlab="",ylab="",xaxt="n", yaxt="n", lty=3,lwd=3,bty='l',
     ylim=c(0,max((foo_20_899$mean+foo_20_899$sd))))
axis(side=1, at=c(0:10))

arrows(c(1:10),foo_20_899$mean-foo_20_899$sd,c(1:10),foo_20_899$mean+foo_20_899$sd,code=3,length=0.02,angle=90,col='orange3')


#lines(rbind(1:10,1:10,NA),rbind(foo_20_899$mean-foo_20_899$sd,foo_20_899$mean+foo_20_899$sd,NA),col="orange3")

par(new=TRUE)

He_40y_899_mean<-colMeans(He_40y_899)
He_40y_899_sd<-colSds(He_40y_899)

foo_40_899 <- data.frame(peak=c(1:10),
                         mean=He_40y_899_mean,
                         sd=He_40y_899_sd)

plot(1:10,foo_40_899$mean,pch=19,type="o" ,col="purple3",xlab="",ylab="",xaxt="n", yaxt="n",lty=3,lwd=3,bty='l',
     ylim=c(0,max((foo_40_899$mean+foo_40_899$sd))))

arrows(c(1:10),foo_40_899$mean-foo_40_899$sd,c(1:10),foo_40_899$mean+foo_40_899$sd,code=3,length=0.02,angle=90,col="purple3")

#lines(rbind(1:10,1:10,NA),rbind(foo_40_899$mean-foo_40_899$sd,foo_40_899$mean+foo_40_899$sd,NA),col="grey")

#https://stackoverflow.com/questions/44847770/split-legend-into-two-lines
legend_order <- matrix(1:6,ncol=1,byrow = T)
legend("bottomleft",c("10-year","20-year","40-year",
                      "low dispersal","medium dispersal","high dispersal")[legend_order],
       lty=c(1,1,1,1,2,3)[legend_order],
       lwd=2,bty="n",cex=1.3,
       col=c("green4",
             "orange3",
             "purple3",
             "black",
             "black",
             "black")[legend_order],
       ncol=1)
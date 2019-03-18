###Isolation by numbers of outbreak cycles

#install.packages("adegenet", dep=TRUE)

library("ade4")
#####################################
#importer les pairwise Fst de 40 ans
directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_40y899.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_51x51_3x3_dis299_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_40y299.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_41x41_3x3_dis199_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_40y199.RData")

####################################
#importer les pairwise Fst de 20 ans
directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_61x61_3x3_dis899_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_20y899.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_31x31_3x3_dis299_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_20y299.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_21x21_3x3_dis199_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_20y199.RData")

####################################
#importer les pairwise Fst de 10 ans
directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_41x41_3x3_dis899_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_10y899.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis299_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_10y299.RData")


directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30/metadat"
setwd(directory)

load("FstP_Mean_pks_10y199.RData")

## create cycle time distance
dist_cycle<-rbind(0:9,c(1,0:8),c(2:1,0:7),c(3:1,0:6),c(4:1,0:5),
                  c(5:1,0:4), c(6:1,0:3), c(7:1,0:2),c(8:1,0:1),9:0)



distCyc<-as.dist(dist_cycle)

#transform Fst to dist data
FstPpks40y899<-as.dist(FstP_Mean_pks_40y899)
FstPpks40y299<-as.dist(FstP_Mean_pks_40y299)
FstPpks40y199<-as.dist(FstP_Mean_pks_40y199)

FstPpks20y899<-as.dist(FstP_Mean_pks_20y899)
FstPpks20y299<-as.dist(FstP_Mean_pks_20y299)
FstPpks20y199<-as.dist(FstP_Mean_pks_20y199)

FstPpks10y899<-as.dist(FstP_Mean_pks_10y899)
FstPpks10y299<-as.dist(FstP_Mean_pks_10y299)
FstPpks10y199<-as.dist(FstP_Mean_pks_10y199)


#isolation by numbers of outbreak cycle
IBCpks40y899<-mantel.randtest(FstPpks40y899, distCyc, nrepet = 999)
IBCpks40y299<-mantel.randtest(FstPpks40y299, distCyc, nrepet = 999)
IBCpks40y199<-mantel.randtest(FstPpks40y199, distCyc, nrepet = 999)

IBCpks20y899<-mantel.randtest(FstPpks20y899, distCyc, nrepet = 999)
IBCpks20y299<-mantel.randtest(FstPpks20y299, distCyc, nrepet = 999)
IBCpks20y199<-mantel.randtest(FstPpks20y199, distCyc, nrepet = 999)

IBCpks10y899<-mantel.randtest(FstPpks10y899, distCyc, nrepet = 999)
IBCpks10y299<-mantel.randtest(FstPpks10y299, distCyc, nrepet = 999)
IBCpks10y199<-mantel.randtest(FstPpks10y199, distCyc, nrepet = 999)

#40year 

plot(dist_cycle, FstP_Mean_pks_40y899,main="Isolation by outbreak cycles", cex.main=2,col="purple3",xlab="",ylab="", 
     lty=3,lwd=1,xlim=c(0,9.2),ylim=c(0,0.35),bty='l')

title(xlab="Number of outbreak cycles between peaks", ylab="pairwise Fst between peaks ", line=2, cex.lab=1.3)

abline(lm(FstPpks40y899~distCyc), col="purple3",lty=3,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_40y299, col="purple3",xlab="",ylab="",xaxt="n", yaxt="n", pch=2,
     lty=3,lwd=1,xlim=c(0,9.2),  ylim=c(0,0.35),bty='l')
abline(lm(FstPpks40y299~distCyc), col="purple3",lty=2,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_40y199, col="purple3", xlab="",ylab="",xaxt="n", yaxt="n", pch=0,
     lty=3,lwd=1,xlim=c(0,9.2),ylim=c(0,0.35),bty='l')
abline(lm(FstPpks40y199~distCyc), col="purple3",lty=1,lwd=3)

#20y
par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_20y899, col="orange3", xlab="",ylab="",xaxt="n", yaxt="n", 
     lty=3,lwd=1, xlim=c(0,9.2),ylim=c(0,0.35),bty='l')
abline(lm(FstPpks20y899~distCyc), col="orange3",lty=3,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_20y299, col="orange3",xlab="",ylab="",xaxt="n", yaxt="n", pch=2,
     lty=3,lwd=1,xlim=c(0,9.2), ylim=c(0,0.35),bty='l')
abline(lm(FstPpks20y299~distCyc), col="orange3",lty=2,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_20y199, col="orange3",xlab="",ylab="",xaxt="n", yaxt="n", pch=0,
     lty=3,lwd=1,xlim=c(0,9.2), ylim=c(0,0.35),bty='l')
abline(lm(FstPpks20y199~distCyc), col="orange3",lty=1,lwd=3)

#10y
par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_10y899, col="green4", xlab="",ylab="",xaxt="n", yaxt="n", 
     lty=3,lwd=1,xlim=c(0,9.2), ylim=c(0,0.35),bty='l')
abline(lm(FstPpks10y899~distCyc), col="green4",lty=3,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_10y299, col="green4", xlab="",ylab="",xaxt="n", yaxt="n", pch=2,
     lty=3,lwd=1,xlim=c(0,9.2), ylim=c(0,0.35),bty='l')
abline(lm(FstPpks10y299~distCyc), col="green4",lty=2,lwd=3)

par(new=TRUE)
plot(dist_cycle, FstP_Mean_pks_10y199, col="green4", xlab="",ylab="",xaxt="n", yaxt="n", pch=0,
     lty=3,lwd=1, xlim=c(0,9.2),ylim=c(0,0.35),bty='l')
abline(lm(FstPpks10y199~distCyc), col="green4",lty=1,lwd=3)

axis(side=1, at=c(0:9.2))


#https://stackoverflow.com/questions/44847770/split-legend-into-two-lines
legend_order <- matrix(1:6,ncol=1,byrow = TRUE)
legend("topleft",c("10-year", "20-year","40-year","low dispersal","medium dispersal","high dispersal")[legend_order],
       lty=c(1,1,1,1,2,3)[legend_order],
       lwd=2,bty="n", cex=1.3,
       col=c("green4",
             "orange3",
             "purple3",
             "black",
             "black",
             "black")[legend_order],
       ncol=1)

#######plot low dispersal

##load 10y scenarios
directory1<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30/heterozygosity"
setwd(directory1)

He_10y199<-read.csv("He_10y199.csv")
He_10y199<-He_10y199[-1]
He_10y199<-colMeans(He_10y199)

directory2<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis299_k30/heterozygosity"
setwd(directory2)

He_10y299<-read.csv("He_10y299.csv")
He_10y299<-He_10y299[-1]
He_10y299<-colMeans(He_10y299)


directory3<-"F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_41x41_3x3_dis899_k30/heterozygosity"
setwd(directory3)

He_10y899<-read.csv("He_10y899.csv")
He_10y899<-He_10y899[-1]
He_10y899<-colMeans(He_10y899)

##load 20y scenarios
directory4<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_21x21_3x3_dis199_k30/heterozygosity"
setwd(directory4)

He_20y199<-read.csv("He_20y199.csv")
He_20y199<-He_20y199[-1]
He_20y199<-colMeans(He_20y199)

directory5<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_31x31_3x3_dis299_k30/heterozygosity"
setwd(directory5)

He_20y299<-read.csv("He_20y299.csv")
He_20y299<-He_20y299[-1]
He_20y299<-colMeans(He_20y299)


directory6<-"F:/Ryan(simulations)/simulation_grid_nolimit/20y_nolimit/20y_61x61_3x3_dis899_k30/heterozygosity"
setwd(directory6)

He_20y899<-read.csv("He_20y899.csv")
He_20y899<-He_20y899[-1]
He_20y899<-colMeans(He_20y899)


##load 40y scenarios
directory7<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_41x41_3x3_dis199_k30/heterozygosity"
setwd(directory7)

He_40y199<-read.csv("He_40y199.csv")
He_40y199<-He_40y199[-1]
He_40y199<-colMeans(He_40y199)

directory8<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_51x51_3x3_dis299_k30/heterozygosity"
setwd(directory8)

He_40y299<-read.csv("He_40y299.csv")
He_40y299<-He_40y299[-1]
He_40y299<-colMeans(He_40y299)


directory9<-"F:/Ryan(simulations)/simulation_grid_nolimit/40y_nolimit/40y_81x81_3x3_dis899_k30/heterozygosity"
setwd(directory9)

He_40y899<-read.csv("He_40y899.csv")
He_40y899<-He_40y899[-1]
He_40y899<-colMeans(He_40y899)


par(mar=c(3,25,2,25))

par(mfrow=c(3,1))

##############################
#### low dispersal

plot(c(1:400),He_40y199,pch=19, main="low dispersal", type="l" ,col="purple3", xlab="year",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

title(xlab="year", ylab="He ", line=2, cex.lab=1)


par(new=TRUE)

plot(c(1:200),He_20y199,pch=19, type="l" ,col="orange3", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')
par(new=TRUE)

plot(c(1:100),He_10y199,pch=19, type="l" ,col="green4", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

axis(1,  at=seq(0, 400, by=10))
axis(2)
legend("bottomleft",c("10-year","20-year","40-year"),lty=1,
       lwd=2,bty="n", 
       col=c("green4","orange3","purple3"),
       ncol=3)



##############################
#### medium dispersal

plot(c(1:400),He_40y299,pch=19,main="medium dispersal", type="l" ,col="purple3", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

title(xlab="year", ylab="He ", line=2, cex.lab=1)


par(new=TRUE)

plot(c(1:200),He_20y299,pch=19, type="l" ,col="orange3", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')
par(new=TRUE)

plot(c(1:100),He_10y299,pch=19, type="l" ,col="green4", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

axis(1,  at=seq(0, 400, by=10))
axis(2)
legend("bottomleft",c("10-year","20-year","40-year"),lty=1,
       lwd=2,bty="n", 
       col=c("green4","orange3","purple3"),
       ncol=3)

##############################
#### high dispersal

plot(c(1:400),He_40y899,pch=19,main="high dispersal", type="l" ,col="purple3", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

title(xlab="year", ylab="He", line=2, cex.lab=1)


par(new=TRUE)

plot(c(1:200),He_20y899,pch=19, type="l" ,col="orange3", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')
par(new=TRUE)

plot(c(1:100),He_10y899,pch=19, type="l" ,col="green4", xlab="",ylab="",xaxt="n", yaxt="n", lty=1,lwd=2,
     ylim=c(0,0.5), xlim=c(0,400),bty='l')

axis(1,  at=seq(0, 400, by=10))
axis(2)
legend("bottomleft",c("10-year","20-year","40-year"),lty=1,
       lwd=2,bty="n", 
       col=c("green4","orange3","purple3"),
       ncol=3)
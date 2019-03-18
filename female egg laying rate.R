directory<-"F:/Ryan(simulations)/simulation_grid_nolimit/egglayrate"
setwd<-directory

load(paste(directory,"/EggLayRate_10y199.RData",sep=""))
load(paste(directory,"/EggLayRate_10y299.RData",sep=""))
load(paste(directory,"/EggLayRate_10y899.RData",sep=""))

load(paste(directory,"/EggLayRate_20y199.RData",sep=""))
load(paste(directory,"/EggLayRate_20y299.RData",sep=""))
load(paste(directory,"/EggLayRate_20y899.RData",sep=""))


load(paste(directory,"/EggLayRate_40y199.RData",sep=""))
load(paste(directory,"/EggLayRate_40y299.RData",sep=""))
load(paste(directory,"/EggLayRate_40y899.RData",sep=""))

par(mar=c(3,25,1,25))
    
attach(mtcars)
par(mfrow=c(3,1))

plot(EggLayRate_10y199[1:11],type="l",col="green4", main="Female egg laying rate ",xlab="",ylab="",
     xaxt="n", yaxt="n",lty=1, ylim=c(0,1), xlim=c(1,10),bty='l',lwd=2)
title(xlab="year", ylab="Female layed eggs / All female ", line=2, cex.lab=1)

par(new=TRUE)
plot(EggLayRate_10y299[1:11],type="l",col="green4", xlab="",ylab="",xaxt="n", yaxt="n",lty=2, ylim=c(0,1), xlim=c(1,10),bty='l',lwd=2)
par(new=TRUE)
plot(EggLayRate_10y899[1:11],type="l",col="green4", xlab="",ylab="",xaxt="n", yaxt="n",lty=3, ylim=c(0,1), xlim=c(1,10),bty='l',lwd=2)
axis(1,  at=seq(0, 10),cex.axis=0.8)
axis(2,cex.axis=0.8)
legend("bottomleft",c("10-year"),lty=c(1),
       lwd=2,bty="n", 
       col="green4",
       ncol=3, cex = 1.25)



plot(EggLayRate_20y199[1:21],type="l",col="orange3", main="",xlab="",ylab="",
     xaxt="n", yaxt="n",lty=1, ylim=c(0,1), xlim=c(1,20),bty='l',lwd=2)
title(xlab="year", ylab="Female layed eggs / All female", line=2, cex.lab=1,font=5)

par(new=TRUE)
plot(EggLayRate_20y299[1:21],type="l",col="orange3", xlab="",ylab="",xaxt="n", yaxt="n",lty=2, ylim=c(0,1), xlim=c(1,20),bty='l',lwd=2)
par(new=TRUE)
plot(EggLayRate_20y899[1:21],type="l",col="orange3", xlab="",ylab="",xaxt="n", yaxt="n",lty=3, ylim=c(0,1), xlim=c(1,20),bty='l',lwd=2)
axis(1,  at=seq(0, 20,by=2),cex.axis=0.8)
axis(2,cex.axis=0.8)
legend("bottomleft",c("20-year"),lty=c(1),
       lwd=2,bty="n", 
       col="orange3",
       ncol=3, cex = 1.25)


plot(EggLayRate_40y199[1:41],type="l",col="purple3", main="",xlab="",ylab="",
     xaxt="n", yaxt="n",lty=1, ylim=c(0,1), xlim=c(1,40),bty='l',lwd=2)
title(xlab="year", ylab="Female layed eggs / All female", line=2, cex.lab=1)

par(new=TRUE)
plot(EggLayRate_40y299[1:41],type="l",col="purple3", xlab="",ylab="",xaxt="n", yaxt="n",lty=2, ylim=c(0,1), xlim=c(1,40),bty='l',lwd=2)
par(new=TRUE)
plot(EggLayRate_40y899[1:41],type="l",col="purple3", xlab="",ylab="",xaxt="n", yaxt="n",lty=3, ylim=c(0,1), xlim=c(1,40),bty='l',lwd=2)
axis(1,  at=seq(0, 40,by=5),cex.axis=0.8)
axis(2,cex.axis=0.8)

legend("bottomleft",c("40-year"),lty=c(1),
       lwd=2,bty="n", 
       col="purple3",
       ncol=3, cex = 1.25)
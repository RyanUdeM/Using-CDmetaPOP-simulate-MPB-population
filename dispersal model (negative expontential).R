disp<- function(a,b,x){ a*10^(b*x)}
par(mar=c(5,5,5,5))
par(mfrow=c(1,1))
plot(disp(100,-0.3, seq(0, 10, 1)),lwd=3, col="black", ylab="", xlab= "",xaxt="n", yaxt="n", type="l")

abline(v=1.99, col="royalblue", lty=2, lwd=3)
abline(v=2.99, col="darkgreen", lty=2, lwd=3)
abline(v=8.99, col="firebrick", lty=2, lwd=3)

title("Exponential distribution",line=1)
title(xlab="Distance (piexl)", ylab="Prob(x)", line=2, cex.lab=1)

axis(1,  at=seq(0,10,by=1),cex.axis=0.8)
axis(2,at=seq(0,100,by=10),cex.axis=0.8)

legend("top", legend=c("low dispersal","medium dispersal","high dispersal"), lty=c(2,2,2), bty="n", 
       lwd=c(2,2,2), col=c("royalblue", "darkgreen", "firebrick"))




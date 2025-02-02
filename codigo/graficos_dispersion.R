dl = c(153,218,361,649,1209,2346,4601)
ln = c(553,846,1483,2710,5162,10057,19871)
dt = c(81,120,201,353,653,1329,2644)
gb = c(277,440,783,1525,3128,6416, 12518)
rf = c(417,581,968,1720,3336,6490,13141)

factors = c(1,2,4,8,16,32,64)

#par(mar=c(5,6,4,2)+1)

pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
par(mar=c(3,7,0,0)+1)

plot(factors, dl, type = "n", 
     las=1,
     xlab="Time series length", 
     ylab = "",
     xaxt = "n",
     cex.lab=2, 
     cex.axis=2,
     ylim = c(0,20000),
     xlim = c(3,62)
     )
mtext("Time (sec)",side=2.2, line=6.5, cex = 2 )
axis(1, at=factors, labels=c("x1","x2","x4","x8","x16","x32","x64"), cex.axis=2 )

#points(factors,dl,cex=1,pch=16,col="black")
lines(x=factors, y= dl, lwd=4)

#points(factors,ln,cex=1,pch=0,col="black")
lines(x=factors, y= ln, lwd=3,lty=2)

#points(factors,dt,cex=1,pch="+",col="black")
lines(x=factors, y= dt, lty=3, lwd=3)

#points(factors,gb,cex=1,pch=17,col="black")
lines(x=factors, y= gb,  lty=5, lwd=2)

#points(factors,rf,cex=1,pch=18,col="black")
lines(x=factors, y= rf, lty=4, lwd=2)

legend("topleft",
       inset=.001,
       cex = 2,
       c("Deep Learning","Linear Regression", "Decision Tree","Gradient-Boosted Trees","Random Forest"),
       horiz=FALSE,
       lty=c(1,2,3,4,5),
       lwd=c(4,2,3,2,2),
       #pch=c(16,0,"+",18,17),
       col=c("black","black"),
       bg="grey96")

dev.off()
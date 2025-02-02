factors = c(1,2,4,8,16,32,64)

hilos.2 = c(595,1195,2389,4823,9276,18244,35802)
hilos.4 = c(327,639,1284,2961,5394,10719,20911)
hilos.6 = c(244,464,915,1837,3763,7438,14489)
hilos.10 = c(232,420,802,1590,3356,6540,13071)
hilos.12 = c(229,384,782,1524,3235,6333,12673)
hilos.8 = c(237,449,872,1725,3579,7034,13929)

#par(mar=c(5,6,4,2)+1)

pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
par(mar=c(3,7,0,0)+1)

#par(mar=c(4,5,0,0)+1)
plot(factors, hilos.2, type = "n", 
     las=1,
     xlab="Time series length", 
     ylab = "",
     xaxt = "n",
     cex.lab=2, 
     cex.axis=2,
     #ylim = c(0,20000),
     xlim = c(3,62)
)
mtext("Time (sec)",side=2.2, line=6.5, cex = 2 )
axis(1, at=factors, labels=c("x1","x2","x4","x8","x16","x32","x64"), cex.axis = 2)

lines(x=factors, y= hilos.2, lwd=2, lty=3)

lines(x=factors, y= hilos.4, lwd=2, lty=2)

lines(x=factors, y= hilos.6, lwd=2, lty=1)

lines(x=factors, y= hilos.8, lwd=2, lty=4)

lines(x=factors, y= hilos.10, lwd=2, lty=5)

lines(x=factors, y= hilos.12, lwd=2, lty=6)

legend("topleft",
       inset=.001,
       cex = 2,
       c("2 threads","4 threads","6 threads","8 threads","10 threads","12 threads"),
       horiz=FALSE,
       lty=c(3,2,1,4,5,6),
       lwd=c(2,2,2,2,2,2),
       #pch=c(16,0,"+",18,17),
       col=c("black","black"),
       bg="grey96")

dev.off()


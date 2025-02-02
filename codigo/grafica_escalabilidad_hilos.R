hilos = c(2,4,6,8,10,12)
dev.off()
x1 = c(595,327,244,237,232,229)
x2 = c(1195,639,464,449,420,384)
x4 = c(2389,1284,915,872,802,782)
x8 = c(4823,2961,1837,1725,1590,1524)
x16 = c(9276,5394,3763,3579,3356,3235)
x32 = c(18244,10719,7438,7034,6540,6333)
x64 = c(35802,20911,14489,13929,13071,12673)

#par(mar=c(2, 1.5, 1.5, 8), mfrow=c(2,1), oma = c(4, 3, 0, 20))
#par(mar=c(6, 3,6, 3), mfrow=c(1,2), oma = c(2, 3, 0, 0))
#nf <- layout(matrix(c(1,2),ncol = 1), widths=c(15,15), heights=c(26,26), FALSE) 


#pdf("/Users/josftm/Downloads/mygraph.pdf", width = 7, height = 5)

pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
par(mar=c(3,7,0,0)+1)

plot(hilos, x1, type = "n", 
     las=1,
     xlab="", 
     ylab = "",
     #xaxt = "n",
     cex.lab=2, 
     cex.axis=2,
     xlim = c(2,12),
     ylim = c(0,5000)
)
mtext("Time (sec)",side=2, line=6, cex = 2 )
mtext("Number of threads",side=1, line=3, cex = 2 )
lines(x=hilos, y=x1, lwd=3, lty=1 )
lines(x=hilos, y=x2, lwd=3, lty=2 )
lines(x=hilos, y=x4, lwd=3, lty=3 )
lines(x=hilos, y=x8, lwd=3, lty=4 )
legend("topright",
       inset=.001,
       cex = 2,
       c("x1", "x2","x4","x8"),
       horiz=FALSE,
       lty=c(1,2,3,4),
       lwd=c(3,3,3,3),
       #pch=c(16,0,"+",18,17),
       col=c("black","black"),
       bg="grey96")

dev.off()

#pdf("/Users/josftm/Downloads/mygraph2.pdf", width = 7, height = 5)
pdf("/Users/josftm/Downloads/image2.pdf",height = 7, width = 12.4)
par(mar=c(3,7,0,0)+1)
##OTRA GRAFICA
plot(hilos, x8, type = "n", 
     las=1,
     xlab="", 
     ylab = "",
     #xaxt = "n",
     cex.lab=2, 
     cex.axis=2,
     xlim = c(2,12),
     ylim = c(3000,37000)
)
mtext("Time (sec)",side=2, line=6, cex = 2 )
mtext("Number of threads",side=1, line=3, cex = 2 )

lines(x=hilos, y=x16, lwd=3, lty=1 )
lines(x=hilos, y=x32, lwd=3, lty=2 )
lines(x=hilos, y=x64, lwd=3, lty=3 )

legend("topright",
       inset=.001,
       cex =2,
       c("x16", "x32","x64"),
       horiz=FALSE,
       lty=c(1,2,3,4),
       lwd=c(3,3,3,3),
       #pch=c(16,0,"+",18,17),
       col=c("black","black"),
       bg="grey96")

dev.off()
## ARCHIVOS
df <- as.matrix(read.csv(file = '/Users/josftm/Documents/lab/ICAEXP/Experimentacion_ICAE/parametros optimos/Lambda0_gaussian/comparison.csv',
               sep = " ",
               header = TRUE
))

real_vector = df[,1]
pred_vector = df[,2]

h = 144

lineas = length(real_vector)/h

## OBTENER EL PEOR Y MEJOR ÍNDICE
real_vector2 = matrix(real_vector,nrow = lineas, byrow = TRUE);
pred_vector2 = matrix(pred_vector, nrow = lineas, byrow = TRUE);
error = abs (real_vector2-pred_vector2)/real_vector2

means = vector()
for (i in c(1:lineas)){
  means[i]= mean(error[i,])
}

# Cambiar el min por max para obtener la mejor o la peor
index = which(means==max(means))

pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
# Cambiar el min por max para obtener la mejor o la peor
par(mar=c(3,7,0,0)+1)
plot (real_vector2[index,], type = "n", 
      las=1,
      xlab="Time (10 min.)", 
      ylab = "",
      cex.lab=2, 
      cex.axis=2,
      ylim = c(17500,27500) 
      #ylim = c(22000,35000) 
      )
mtext("Electricity consumption",side=2.2, line=6.5, cex = 2 )
lines(real_vector2[index,], lwd=3)
lines(pred_vector2[index,], lwd=1, col="black")
legend("topleft",
       inset=.001,
       cex = 2,
       c("Actual data","Predicted data"),
       horiz=FALSE,
       #lty=c(1,4),
       lwd=c(3,1),
       pch=c(NA,NA),
       col=c("black","black"),
       bg="grey96")

dev.off()

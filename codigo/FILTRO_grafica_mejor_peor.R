## ARCHIVOS
df <- as.matrix(read.csv(file = '/Users/josftm/Downloads/comparison.csv',
               sep = " ",
               header = TRUE
))

real_vector = df[,1]
pred_vector = df[,2]

errorSinFiltro = sum(abs(real_vector-pred_vector)/real_vector)/length(real_vector)


# APLICACION DEL FILTRO QUITANDO LOS 3 PRIMEROS Y 3 ULTIMOS VALORES PORQUE EL FILTRO NO LOS IDENTIFICA
pred_vector = stats:::filter(x= pred_vector, filter = c(1,1,1,1,1,1,1)/7) 
real_vector = real_vector[4:(length((real_vector))-3)]
pred_vector = pred_vector[4:(length((pred_vector))-3)]
# FIN DEL FILTRO

errorFiltro = sum(abs(real_vector-pred_vector)/real_vector)/length(real_vector)

dif = (errorSinFiltro-errorFiltro)/errorSinFiltro

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
index = which(means==min(means))

#pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
# Cambiar el min por max para obtener la mejor o la peor
par(mar=c(3,5,0,0)+1)
plot (real_vector2[index,], type = "n", 
      las=1,
      xlab="Time (10 min.)", 
      ylab = "",
      cex.lab=1, 
      cex.axis=1,
      #ylim = c(17500,27500) 
      #ylim = c(22000,35000) 
      )
mtext("Electricity consumption",side=2, line=4.5, cex = 1 )
lines(real_vector2[index,], lwd=3)
lines(pred_vector2[index,], lwd=1, col="black")
legend("bottomleft",
       inset=.001,
       #cex = 1.4,
       c("Actual data","Predicted data"),
       horiz=FALSE,
       #lty=c(1,4),
       lwd=c(3,1),
       pch=c(NA,NA),
       col=c("black","black"),
       bg="grey96")
#dev.off()

print(errorFiltro)
print(errorSinFiltro)
print(dif*100)


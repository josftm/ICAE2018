## ARCHIVOS
df <- as.matrix(read.csv(file = '/Users/josftm/Documents/Experimentacion ICAE/parametros optimos/Lambda0_gaussian/comparison.csv',
                         sep = " ",
                         header = TRUE
))

#real_vector = df[,1]
#pred_vector = df[,2]

#Para el mes de mayo de 2016
real_vector = df[141818:146281,1]
pred_vector = df[141818:146281,2]

#Para el mes de Abril de 2016
real_vector = df[137498:141817,1]
pred_vector = df[137498:141817,2]

#Para el mes de Abril mayo de 2016
real_vector = df[137498:146281,1]
pred_vector = df[137498:146281,2]

h = 144

lineas = length(real_vector)/h

## OBTENER EL PEOR Y MEJOR ÍNDICE
real_vector2 = matrix(real_vector,nrow = lineas, byrow = TRUE);
pred_vector2 = matrix(pred_vector, nrow = lineas, byrow = TRUE);

means_reales = vector()
means_predichos = vector()
for (i in c(1:lineas)){
  means_reales[i]= mean(real_vector2[i,])
  means_predichos[i]= mean(pred_vector2[i,])
}

pdf("/Users/josftm/Downloads/image.pdf",height = 7, width = 12.4)
# Cambiar el min por max para obtener la mejor o la peor
par(mar=c(3,7,0,0)+1)
plot ( means_reales, type = "n", 
      las=1,
      xlab="Days", 
      ylab = "",
      cex.lab=2, 
      cex.axis=2,
      #ylim = c(17500,27500) 
      ylim = c(22000,31000) 
)
mtext("Electricity consumption",side=2.2, line=6.5, cex = 2 )
lines( means_reales, lwd=1, type="o", pch=10 )
lines(means_predichos, lwd=2, col="black")
legend("topright",
       inset=.001,
       cex = 2,
       c("Actual data","Predicted data"),
       horiz=FALSE,
       #lty=c(1,4),
       lwd=c(1,2),
       pch=c(10,NA),
       col=c("black","black"),
       bg="grey96")
dev.off()

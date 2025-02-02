fileName <- "/Users/josftm/Documents/lab/SparkR/Experimentacion_ICAE/parametros optimos/Lambda0.001_poisson/models copia.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)
close(conn)

vec_RMSE = vector()
vec_MAE = vector()

library(tidyr)

length(as.vector(extract_numeric(linn[which(grepl("RMSE", linn))])))
length(as.vector(extract_numeric(linn[which(grepl("MAE", linn))])))

mean_RMSE = mean(as.vector(extract_numeric(linn[which(grepl("RMSE", linn))])))
mean_MAE = mean(as.vector(extract_numeric(linn[which(grepl("MAE", linn))])))

mean_RMSE
mean_MAE

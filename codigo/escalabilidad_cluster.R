# EN TODAS LAS MAQUINAS DEL CLUSTER:
# - CREAR CARPETA /HOME/JOSFTM/DOCUMENTOS/ICAE
# - CREAR CARPETA /HOME/JOSFTM/DOCUMENTOS/ICAE/DATOS
# - PEGAR LOS DATOS REDISTRIBUIDOS CON TODOS LOS FACTORES (NOMBRES: MATRIX_X1.CSV, MATRIX_X2.CSV ....)

options( java.parameters = "-Xmx16g" )
library(h2o)
library(rsparkling)
library(sparklyr)
library(dplyr)

listaNeuronas = list(c(80,80),c(100,100),c(60,60,60,60,60),c(60,60,60),c(80,80,80),c(60,60),c(70,70,70),
                     c(80,80,80,80,80),c(100,100),c(60,60,60,60),c(70,70,70),c(100,100,100),c(40,40,40,40),
                     c(70,70,70,70,70),c(70,70),c(70,70),c(50,50,50,50),c(80,80,80,80),c(90,90,90,90),
                     c(50,50,50,50),c(90,90,90,90),c(80,80),c(90,90,90,90),c(100,100,100,100,100))

#Establecer versiones de Spark
spark_install(version = "2.0.2")
options(rsparkling.sparklingwater.version = "2.0.2")

#Tamaño de ventana
w = 168
#Horizonte de prediccion
h = 24

dir = "/home/josftm/Documentos/ICAE/24hilos"
dir.create(dir)

#for(factor.rep in c("matriz_x1","matriz_x2","matriz_x4","matriz_x8","matriz_x16","matriz_x32","matriz_x64")){
for(factor.rep in c("matriz_x64")){
  #Crear conexion con Spark
  options(rsparkling.sparklingwater.version = "2.0.2")
  sc <- spark_connect(master = "spark://torres:7077", version = "2.0.2",
                      config = list("sparklyr.shell.executor-memory"="16g", 
                                    "sparklyr.shell.driver-memory"="16g",
                                    "spark.executor.memory"="16g"
                                    )
                     )
  
  #h2o_context(sc)
  h2o_context(spark_connection(sc))
  h2o_flow(sc)
  
  
  
  #Cargar los datos en un SparkDataFrame y convertirlos a H2OFrame desde la carpeta de datos que ya tenia de ejecuciones anteriores
  data_df <- spark_read_csv(sc, "dataset", sprintf("/home/josftm/Documentos/ICAE/datos/%s.csv",factor.rep),
                            header=FALSE, delimiter = " ")
  data_hf <- as_h2o_frame(sc, data_df)
  
  #Separar training y test
  split = 0.7
  corte = floor(split*nrow(data_hf))
  data.training = data_hf[1:corte,]
  data.test = data_hf[(corte+1):nrow(data_hf),]
  #Separar en training y validation
  corte.val = floor(split*nrow(data.training))
  data.traininVal = data.training[1:corte.val,]
  data.validation = data.training[(corte.val+1):nrow(data.training),]
  
  #Establecer variables predictoras
  predictors = c(1:w)
  
  path = sprintf("%s/Result_%s",dir,factor.rep)
  dir.create(path)
  
  #Variables para almacenar informacion
  info = list()
  errors = NULL
  mejores = vector()
  predicciones = vector()
  pre.agreg = NULL
  errores.validacion.subproblemas = vector()
  #Split en subproblemas
  tiempo.modelos = 0
  tiempo.prediccion = 0
  
  tiempos.subproblemas.train = NULL
  tiempos.subproblemas.predict = NULL
  
  for(i in 1:h){
    neurons = as.list(listaNeuronas[i])
    lambda = 0
    rho = 0.99
    epsilon = 1e-9
    activation = "Tanh"
    distribution = "gaussian"
    stop.metric = "MSE"
    hyper_params <- list(hidden = neurons, 
                         l1 = lambda, 
                         rho = rho, 
                         epsilon = epsilon, 
                         activation =activation,
                         distribution = distribution,
                         stopping_metric=stop.metric
    )
    
    #Etiqueta clase
    y <- w+i
    
    time.start.model = proc.time()
    #Se calcula el modelo
    model_grid = h2o.grid("deeplearning",
                          hyper_params = hyper_params,
                          x = predictors,
                          y = y,
                          training_frame = data.traininVal,
                          validation_frame = data.validation
    )
    
    best_model = h2o.getModel(model_grid@model_ids[[1]])
    time.stop.model = proc.time()
    errores.validacion.subproblemas = append(errores.validacion.subproblemas, best_model@model$validation_metrics@metrics$rmsle * 100)
    tiempo.modelos = tiempo.modelos+(time.stop.model[3]-time.start.model[3])
    #Obtener el mejor modelo
    mejores = append(mejores, best_model)
    
    data.w = data.test
    
    #Predecir
    time.start.predict = proc.time()
    pre = h2o.predict(object = best_model, newdata = data.w)
    time.stop.predict = proc.time()
    tiempo.prediccion = tiempo.prediccion+(time.stop.predict[3]-time.start.predict[3])
    predicciones = append(predicciones,pre)
    
    tiempos.subproblemas.train = rbind(tiempos.subproblemas.train, (time.stop.model[3]-time.start.model[3]))
    tiempos.subproblemas.predict =rbind(tiempos.subproblemas.predict, (time.stop.predict[3]-time.start.predict[3]))
    
    #Calculo de los errores
    mse = 1/nrow(pre)*sum((data.test[,y]-pre)^2)
    rmse = sqrt(1/nrow(pre)*sum((data.test[,y]-pre)^2))
    mae = 1/nrow(pre)*sum(abs((data.test[,y]-pre))^2)
    rel = ( 1 / nrow(pre)) * sum( abs(( data.test[,y] - pre )) / data.test[,y]) * 100
    
    pre.agreg = cbind(pre.agreg, as.vector(pre))
    
    errors = rbind(errors, c(y,mse, rmse, mae, rel))
    
    #Borrar los modelos
    rm(model_grid)
    rm(best_model)
  }
  
  
  #Volcar los mejores modelos de cada h en archivo/home/josftm/Documentos/lab/SparkR/Datos/electricidad_spain/LOCAL_H24_W168_12HILOS/matriz_x64/redistribuido.csv
  cat(NULL,file=sprintf("%s/models.txt",path))
  sink(sprintf("%s/models.txt",path)) 
  print(mejores)
  sink()
  
  class_test = as.matrix(data.test[,(w+1):ncol(data.test)])
  
  results = as.matrix(as.vector(t(class_test)))
  results = cbind(results, as.vector(t(pre.agreg)))
  colnames(results) <- c("REAL","PREDICTION")
  
  err.accum = NULL
  
  #Calcular errores acumulados de toda la matriz
  mse_acum = 1/(nrow(pre)*ncol(pre))*sum(sum((class_test-pre.agreg)^2))
  rmse_acum = sqrt(1/(nrow(pre)*ncol(pre))*sum(sum((class_test-pre.agreg)^2)))
  mae_acum = 1/(nrow(pre)*ncol(pre))*sum(sum(abs((class_test-pre.agreg))^2))
  rel_acum = 1 / (nrow(pre.agreg)*ncol(pre.agreg)) * sum(sum(abs(pre.agreg - class_test ) / class_test)) * 100
  
  err.accum = rbind(err.accum, c(mse_acum, rmse_acum, mae_acum, rel_acum))
  colnames(err.accum) = c("MSE","RMSE", "MAE", "RELAT")
  
  colnames(errors) = c("H","MSE","RMSE", "MAE", "RELAT")
  #write.table(pre.agreg, file=sprintf("%s/prediccion_as_matrix.csv",path), row.names=FALSE, col.names = FALSE, sep = " ")
  #write.table(results, file=sprintf("%s/comparison.csv",path), row.names=FALSE, col.names = TRUE, sep = " ")
  
  info["MEDIA_RMSLE_VALIDACION"] = mean(errores.validacion.subproblemas)
  info["NEURONAS"] = list(neurons)
  info["LAMBDA"] = lambda
  info["RHO"] = rho
  info["EPSILON"] = epsilon
  info["ACTIVATION"] = activation
  info["DISTRIBUTION"] = distribution
  info["STOPPING_METRIC"] = stop.metric
  info["ERRORS_PER_SUBPROBLEM"] = list(errors)
  info["GLOBAL_ERRORS"] = list(err.accum)
  info["GET_MODEL_TIME"] =  tiempo.modelos 
  info["PREDICTION_TIME"] =  tiempo.prediccion
  info["TIME_EACH_SUBPROBLEM_TRAIN"]= list(tiempos.subproblemas.train)
  info["TIME_EACH_SUBPROBLEM_PREDICT"]= list(tiempos.subproblemas.predict)
  
  sink(sprintf("%s/results.txt",path)) 
  print(info)
  sink()
  
  spark_disconnect_all()
  
}
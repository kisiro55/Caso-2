library (C50)
library(foreign)
library(caret)
library(dplyr)
library(tidyverse)



## FUNCION PARA ENTRENAMIENTO

Entrenamiento <- function(DFTrain, DFTags, ConfLev, MinNum){
  modelo <- C5.0(DFTrain,DFTags, control = C5.0Control(CF = ConfLev,
                                             minCases = MinNum))
  return(modelo)
}

# TRATAMIENTO DE VARIABLES (PENDIENTE)


# OPCION 1 : DATA SET COMPLETO

# OPCION 2 : CATEGORIZAR ALGUNAS VARIABLES

Subs.Train.V2 <- subscriptores.training
Subs.Test.V2 <- subscriptores.test

## MODIFICACION DE LA VARIABLLE DE LLAMADAS AL CENTRO DE CONTACTO
Subs.Train.V2$CustServ_Calls <- as.factor(Subs.Train.V2$CustServ_Calls)
Subs.Test.V2$CustServ_Calls <- as.factor(Subs.Test.V2$CustServ_Calls)

levels(Subs.Train.V2$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

Subs.Train.V2$CustServ_Calls <- ifelse(Subs.Train.V2$CustServ_Calls %in% normales, "Normales", "Quejosos")
Subs.Test.V2$CustServ_Calls <- ifelse(Subs.Test.V2$CustServ_Calls %in% normales, "Normales", "Quejosos")


# QUITAR VARIABLES QUE NO SUMAN 

Subs.Train.V2 <-  select( Subs.Train.V2,-Phone, -Mahalanobis,- Outlier, -Test_Split)
Subs.Test.V2 <-  select( Subs.Test.V2,-Phone, -Mahalanobis,- Outlier, -Test_Split)

# OPCION C : AGRUPAR VARIABLES CUALI EN CUANTI CADA UNA POR SEPARADO.

################################################
#                                              #
# ESTANDARIZACION DE VARIABLES NUMERICAS       #
#                                              #
################################################

<<<<<<< HEAD
#####################   OPCION 1    ################################
=======
## TRAINING
>>>>>>> 80601f6b03dfe7ef7831a32acc6dff9fe2d74115
# Estandarizo el training
idx <- which(sapply(subscriptores.training[,1:17], class) %in% c("numeric","integer"))
Sub_Train_EST1 <- scale(subscriptores.training[,idx])
Sub_Train_EST1 <- cbind(Sub_Train_EST1,subscriptores.training[,-idx] ) 

#Elimino variable Phone (por ser simil a un ID)
Sub_Train_EST1 <- Sub_Train_EST1[,-14]

## TEST
#Estandarizar Test
idx <- which(sapply(subscriptores.test[,1:16], class) %in% c("numeric","integer"))
Sub_Test_EST1 <- scale(subscriptores.test[,idx])
Sub_Test_EST1 <- cbind(Sub_Test_EST1,subscriptores.test[,-idx] ) 

#Elimino variable Phone(por ser simil a un ID)
Sub_Test_EST1 <- Sub_Test_EST1[,-14]

<<<<<<< HEAD
#####################   OPCION 2    ################################
# Estandarizo el training
idx <- which(sapply(Subs.Train.V2[,1:ncol(Subs.Train.V2)], class) %in% c("numeric","integer"))
Sub_Train_EST2 <- scale(Subs.Train.V2[,idx])
Sub_Train_EST2 <- cbind(Sub_Train_EST2,Subs.Train.V2[,-idx] ) 

#Estandarizar Test
idx <- which(sapply(Subs.Test.V2[,1:ncol(Subs.Test.V2)], class) %in% c("numeric","integer"))
Sub_Test_EST2 <- scale(Subs.Test.V2[,idx])
Sub_Test_EST2 <- cbind(Sub_Test_EST2,Subs.Test.V2[,-idx] ) 

=======

# Chequea igual cant de columnas
dim(Sub_Test_EST1)[2] - dim(Sub_Train_EST1)[2]

# Para la realizacion del primer modelo se va proceder a utilizar TODAS las variables 
Modelo1 <- Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, 0.25, 2)

summary(Modelo1)

# plot(Modelo1) # no funciona, no se por que. abajo probe de otra manera y funciona.
# Se procede a probar con el subset de Test
p1 <- predict(Modelo1, Sub_Test_EST1[1:15])

summary(p1)

### No me funciono el plot y probe la formula directa. funciona el plot (es horrible) pero le tengo que agregar todavia los parametros --> chequear
tree_mod <- C5.0(x = Sub_Train_EST1[,1:15], y = Sub_Train_EST1$Churn)

summary(tree_mod)
plot(tree_mod)
>>>>>>> 80601f6b03dfe7ef7831a32acc6dff9fe2d74115

################################################
#
#       FUNCION PARA ENCONTRAR MODELO OPTIMO
#
#################################################

# LA IDEA ES ITERAR MODIFICANDO EL CONF LEVEL Y EL MIN CASES, COLOCAR EN UNA TABLA LA INFORMACION Y MOSTRAR GRAFICAS
# PARA DETERMINAR EL ARBOL IDEAL

# Itero de 1 a 5 Min cases, para cada iteracion se guardara
# TAMAÑO DEL ARBOL
# ACCURACY
# RECALL
# CF
# MIN NUM

# Se crean los Niveles de Confianza para iterar
ConfBase <- seq(0.05, 0.9, by = 0.05)
Resultados <- 0

## EL NOMBRE DE LA COLUMNA ES LA QUE VA A MOSTRAR DE LA MATR CONF

ModeloITE<- list()

######
# ITERACION CON DATA SET 1
#####

pos <- 0

for (i in 1:10){
        for (j in 1:length(ConfBase)) {
                Conf  <- ConfBase[j]
                ModeloITE <-  Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, Conf,i )  #GENERAR MODELO
                PredITE <- predict(ModeloITE, Sub_Test_EST1[1:15])
                matrizConf <- confusionMatrix(data = PredITE, reference = Sub_Test_EST1$Churn, positive = "True.")
          
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass,matrizConf$overall )                                                      
                Resultados <- rbind(Resultados,Datos)   
                pos <- pos + 1
                }
}
Resultados <- Resultados[-c(1),] #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO


head(Resultados)

## Grafico, deteccion de churners vs Accuracy 

######
# ITERACION CON DATA SET 2
#####
Resultadosv2 <- 0
pos <- 0

for (i in 1:10){
        for (j in 1:length(ConfBase)) {
                Conf  <- ConfBase[j]
                ModeloITE <-  Entrenamiento(Sub_Train_EST2[1:15],Sub_Train_EST2$Churn, Conf,i )            #GENERAR MODELO
                PredITE <- predict(ModeloITE, Sub_Test_EST2[1:15])
                
                matrizConf <- confusionMatrix(data = PredITE, reference = Sub_Train_EST2$Churn, positive = "True.")
                
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass,matrizConf$overall )                                                      
                Resultadosv2 <- rbind(Resultadosv2,Datos)   
                pos <- pos + 1
        }
}
Resultadosv2 <- Resultadosv2[-c(1),] #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO


head(Resultadosv2)

## SE DEBERIA ELEGIR EL QUE MEJOR ELIJA A LOS POSIBLES CHURN


## HACER UN SCORING




################################################
#
#                       AUX
#
#################################################

# Para la realizacion del primer modelo se va proceder a utilizar TODAS las variables 
Modelo1 <- Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, 0.25,2 )

Modelo1$control$CF

summary(Modelo1)

# Se procede a probar con el subset de Test
p1 <- predict(Modelo1, Sub_Test_EST1[1:15])
matriz <- confusionMatrix(data = p1, reference = Sub_Test_EST1$Churn, positive = "True.")
summary(p1)


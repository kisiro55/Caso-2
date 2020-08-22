library (C50)
library(foreign)
library(caret)

## FUNCION PARA ENTRENAMIENTO

Entrenamiento <- function(DFTrain, DFTags, ConfLev, MinNum){
  modelo <- C5.0(DFTrain,DFTags, control = C5.0Control(CF = ConfLev,
                                             minCases = MinNum))
  return(modelo)
}



####
# Agrupar variables para simplificar (PENDIENTE)
#

################################################
#                                              #
# ESTANDARIZACION DE VARIABLES NUMERICAS       #
#                                              #
################################################

# Estandarizo el training
idx <- which(sapply(subscriptores.training[,1:17], class) %in% c("numeric","integer"))
Sub_Train_EST1 <- scale(subscriptores.training[,idx])
Sub_Train_EST1 <- cbind(Sub_Train_EST1,subscriptores.training[,-idx] ) 
#Elimino varibale Phone
Sub_Train_EST1 <- Sub_Train_EST1[,-14]

#Estandarizar Test
idx <- which(sapply(subscriptores.test[,1:16], class) %in% c("numeric","integer"))
Sub_Test_EST1 <- scale(subscriptores.test[,idx])
Sub_Test_EST1 <- cbind(Sub_Test_EST1,subscriptores.test[,-idx] ) 
#Elimino varibale Phone
Sub_Test_EST1 <- Sub_Test_EST1[,-14]

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
# ITERACION CON 1ER DATA SET (VIENE DIRECTO DEL SCRIPT DE JUAN), EN EL PASO ANTERIOR HABRIA QUE ARMAR 2 DS MAS CON CONDICIONES DIFERENTES
#

pos <- 0

for (i in 1:10){
        for (j in 1:length(ConfBase)) {
                Conf  <- ConfBase[j]
                ModeloITE <-  Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, Conf,i )  #GENERAR MODELO
                PredITE <- predict(ModeloITE, Sub_Test_EST1[1:15])
                matrizConf <- confusionMatrix(data = PredITE, reference = Sub_Test_EST1$Churn, positive = "True.")
          
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass)                                                      
                Resultados <- rbind(Resultados,Datos)   
                pos <- pos + 1
                }
}
Resultados <- Resultados[-c(1),] #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO

head(Resultados)


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


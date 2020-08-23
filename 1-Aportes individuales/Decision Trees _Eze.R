library (C50)
library(foreign)
library(caret)
library(dplyr)
library(tidyverse)
library(states)
library(gtools)

## FUNCION PARA ENTRENAMIENTO

Entrenamiento <- function(DFTrain, DFTags, ConfLev, MinNum){
  modelo <- C5.0(DFTrain,DFTags, control = C5.0Control(CF = ConfLev,
                                             minCases = MinNum))
  return(modelo)
}
############################   OPCION 1 : DATASET COMPLETO  #######################################################

subscriptores.training <- subscr_training_v1
subscriptores.test <- subscr_test_v1

############################   OPCION 2 : CATEGORIZAR ALGUNAS VARIABLES ############################################

Subs.Train.V2 <- subscr_training_v1
Subs.Test.V2 <- subscr_test_v1

## MODIFICACION DE LA VARIABLLE DE LLAMADAS AL CENTRO DE CONTACTO
Subs.Train.V2$CustServ_Calls <- as.factor(Subs.Train.V2$CustServ_Calls)
Subs.Test.V2$CustServ_Calls <- as.factor(Subs.Test.V2$CustServ_Calls)

levels(Subs.Train.V2$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

Subs.Train.V2$CustServ_Calls <- ifelse(Subs.Train.V2$CustServ_Calls %in% normales, "Normales", "Quejosos")
Subs.Test.V2$CustServ_Calls <- ifelse(Subs.Test.V2$CustServ_Calls %in% normales, "Normales", "Quejosos")

## MODIFICACION DE LA ESTADO CON LA REGION 
Estados <- data.frame(state.abb, state.region )

Subs.Train.V2$State <- ifelse(Subs.Train.V2$State %in% Estados$state.abb, as.character(Estados$state.region),
                              as.character(Subs.Train.V2$State))
Subs.Test.V2$State <- ifelse(Subs.Test.V2$State %in% Estados$state.abb, as.character(Estados$state.region),
                              as.character(Subs.Test.V2$State))

## MODIFICACION DE MINUTOS HABLADOS POR CUARTILES

# DEL SUMMARY SE OBSERVA QUE LAS VARIABLES CUMPLEN CON LA NORMALIDAD.
## TRAINING
quartilesDAY <- quantile(Subs.Train.V2$Day_Mins)
Subs.Train.V2$Day_Mins <- cut(Subs.Train.V2$Day_Mins, breaks= quartilesDAY, labels = c("Consumo_DIA_BAJO", "Consumo_DIA_MEDIO","Consumo_DIA_NORMAL", "Consumo_DIA_ALTO"))
Subs.Train.V2$Day_Mins [is.na(Subs.Train.V2$Day_Mins )] <- "Consumo_DIA_ALTO"

quartilesEVE <- quantile(Subs.Train.V2$Eve_Mins)
Subs.Train.V2$Eve_Mins <- cut(Subs.Train.V2$Eve_Mins, breaks= quartilesDAY, labels = c("Consumo_EVE_BAJO", "Consumo_EVE_MEDIO","Consumo_EVE_NORMAL", "Consumo_EVE_ALTO"))
Subs.Train.V2$Eve_Mins [is.na(Subs.Train.V2$Eve_Mins )] <- "Consumo_EVE_ALTO"

quartilesNIGHT <- quantile(Subs.Train.V2$Night_Mins)
Subs.Train.V2$Night_Mins <- cut(Subs.Train.V2$Night_Mins, breaks= quartilesDAY, labels = c("Consumo_NIGHTBAJO", "Consumo_NIGHT_MEDIO","Consumo_NIGHT_NORMAL", "Consumo_NIGHT_ALTO"))
Subs.Train.V2$Night_Mins [is.na(Subs.Train.V2$Night_Mins )] <- "Consumo_NIGHT_ALTO"

quartilesITL <- quantile(Subs.Train.V2$Intl_Mins)
Subs.Train.V2$Intl_Mins<- cut(Subs.Train.V2$Intl_Mins, breaks= quartilesDAY, labels = c("Consumo_INT_BAJO", "ConsumoINT_MEDIO","Consumo_INT_NORMAL", "Consumo_INT_ALTO"))
Subs.Train.V2$Intl_Mins [is.na(Subs.Train.V2$Intl_Mins )] <- "Consumo_INT_ALTO"

## TEST
quartilesDAY <- quantile(Subs.Test.V2$Day_Mins)
Subs.Test.V2$Day_Mins <- cut(Subs.Test.V2$Day_Mins, breaks= quartilesDAY, labels = c("Consumo_DIA_BAJO", "Consumo_DIA_MEDIO","Consumo_DIA_NORMAL", "Consumo_DIA_ALTO"))
Subs.Test.V2$Day_Mins [is.na(Subs.Test.V2$Day_Mins )] <- "Consumo_DIA_ALTO"

quartilesEVE <- quantile(Subs.Test.V2$Eve_Mins)
Subs.Test.V2$Eve_Mins <- cut(Subs.Test.V2$Eve_Mins, breaks= quartilesDAY, labels = c("Consumo_EVE_BAJO", "Consumo_EVE_MEDIO","Consumo_EVE_NORMAL", "Consumo_EVE_ALTO"))
Subs.Test.V2$Eve_Mins [is.na(Subs.Test.V2$Eve_Mins )] <- "Consumo_EVE_ALTO"

quartilesNIGHT <- quantile(Subs.Test.V2$Night_Mins)
Subs.Test.V2$Night_Mins <- cut(Subs.Test.V2$Night_Mins, breaks= quartilesDAY, labels = c("Consumo_NIGHTBAJO", "Consumo_NIGHT_MEDIO","Consumo_NIGHT_NORMAL", "Consumo_NIGHT_ALTO"))
Subs.Test.V2$Night_Mins [is.na(Subs.Test.V2$Night_Mins )] <- "Consumo_NIGHT_ALTO"

quartilesITL <- quantile(Subs.Test.V2$Intl_Mins)
Subs.Test.V2$Intl_Mins<- cut(Subs.Test.V2$Intl_Mins, breaks= quartilesDAY, labels = c("Consumo_INT_BAJO", "ConsumoINT_MEDIO","Consumo_INT_NORMAL", "Consumo_INT_ALTO"))
Subs.Test.V2$Intl_Mins [is.na(Subs.Test.V2$Intl_Mins )] <- "Consumo_INT_ALTO"


## TEST DE CORRESPONDENCIA 

# JUNTO LOS DS PARA EL ANALISIS

Subs.UNION <- rbind(Subs.Train.V2,Subs.Test.V2)

cols <- which(sapply(Subs.UNION[,1:16], class) %in% c("character"))

Subs.UNION <- Subs.UNION[,cols]

Subs.UNION.mca <- MCA(Subs.UNION,  graph = FALSE)

get_eigenvalue(Subs.UNION.mca)
fviz_eig(Subs.UNION.mca)
get_mca_ind(Subs.UNION.mca)
get_mca_var(Subs.UNION.mca)
fviz_mca_ind(Subs.UNION.mca)
fviz_mca_var(Subs.UNION.mca)

fviz_mca_biplot(Subs.UNION.mca)

# QUITAR VARIABLES QUE NO SUMAN 

Subs.Train.V2 <-  select( Subs.Train.V2,-X1,-Phone, -Mahalanobis,- Outlier, -Test_Split)
Subs.Test.V2 <-  select( Subs.Test.V2,-X1,-Phone, -Mahalanobis,- Outlier, -Test_Split)

############################   OPCION 3 : CATEGORIZAR ALGUNAS VARIABLES ############################################

Subs.Train.V3 <- as.data.frame(subs_train2)
Subs.Test.V3 <- as.data.frame(subs_test2)

Subs.Train.V3 <- Subs.Train.V3 %>% mutate_if(is.character,as.factor)
Subs.Train.V3$Ambos_planes <- as.factor(Subs.Train.V3$Ambos_planes)

Subs.Test.V3 <- Subs.Test.V3 %>% mutate_if(is.character,as.factor)
Subs.Test.V3$Ambos_planes <- as.factor(Subs.Test.V3$Ambos_planes)

summary(Subs.Train.V3)

################################################
#                                              #
# ESTANDARIZACION DE VARIABLES NUMERICAS       #
#                                              #
################################################

#####################   OPCION 1    ################################
## TRAINING
# Estandarizo el training
idx <- which(sapply(subscriptores.training[,1:17], class) %in% c("numeric","integer"))
Sub_Train_EST1 <- scale(subscriptores.training[,idx])
Sub_Train_EST1 <- cbind(Sub_Train_EST1,subscriptores.training[,-idx] ) 


## TEST
#Estandarizar Test
idx <- which(sapply(subscriptores.test[,1:16], class) %in% c("numeric","integer"))
Sub_Test_EST1 <- scale(subscriptores.test[,idx])
Sub_Test_EST1 <- cbind(Sub_Test_EST1,subscriptores.test[,-idx] ) 

#Elimino variable Phone(por ser simil a un ID)

Sub_Test_EST1 <-  select( Sub_Test_EST1,-X1,-Phone, -Mahalanobis,- Outlier, -Test_Split)
Sub_Train_EST1 <-  select(Sub_Train_EST1,-X1,-Phone, -Mahalanobis,- Outlier, -Test_Split)

#####################   OPCION 2    ################################
# Estandarizo el training
idx <- which(sapply(Subs.Train.V2[,1:ncol(Subs.Train.V2)], class) %in% c("numeric","integer"))
Sub_Train_EST2 <- scale(Subs.Train.V2[,idx])
Sub_Train_EST2 <- cbind(Sub_Train_EST2,Subs.Train.V2[,-idx] ) 

#Estandarizar Test
idx <- which(sapply(Subs.Test.V2[,1:ncol(Subs.Test.V2)], class) %in% c("numeric","integer"))
Sub_Test_EST2 <- scale(Subs.Test.V2[,idx])
Sub_Test_EST2 <- cbind(Sub_Test_EST2,Subs.Test.V2[,-idx] ) 


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
                ModeloITE <-  Entrenamiento(Sub_Train_EST1[1:15],as.factor(Sub_Train_EST1$Churn), Conf,i )  #GENERAR MODELO
                PredITE <- predict(ModeloITE, Sub_Test_EST1)
                matrizConf <- confusionMatrix(data = PredITE, reference = as.factor(Sub_Test_EST1$Churn), positive = "True.")
          
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass,matrizConf$overall )                                                      
                Resultados <- rbind(Resultados,Datos)   
                pos <- pos + 1
                }
}
Resultados <- Resultados[-c(1),] #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO

head(Resultados)

x <- c(1,2,8,9,14)
data <- Resultados[,x]
head(data)
data <- as.data.frame(data)
data[,3] <- round(data[,3], 2)
data[,4] <- round(data[,4], 2)
data[,5] <- round(data[,5], 2)
## Plot
library(ggplot2) # appearance
library(GGally)
ggparcoord(data,
           columns = 1:5, groupColumn = 1, scale="center",
           alphaLines = 0.9) +
        scale_color_viridis(discrete=F)

## Grafico, deteccion de churners vs Accuracy 

################################################
# ITERACION CON DATA SET 2
################################################

Resultadosv2 <- 0
pos <- 0

for (i in 1:10){
        for (j in 1:length(ConfBase)) {
                Conf  <- ConfBase[j]
                ModeloITE <-  Entrenamiento(Sub_Train_EST2[1:15],Sub_Train_EST2$Churn, Conf,i )            #GENERAR MODELO
                PredITE <- predict(ModeloITE, Sub_Test_EST2[1:15])
                
                matrizConf <- confusionMatrix(data = PredITE, reference = Sub_Test_EST2$Churn, positive = "True.")
                
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass,matrizConf$overall )                                                      
                Resultadosv2 <- rbind(Resultadosv2,Datos)   
                pos <- pos + 1
        }
}
Resultadosv2 <- Resultadosv2[-c(1),]  #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO


head(Resultadosv2)

################################################
# ITERACION CON DATA SET 3
################################################

Resultadosv3 <- 0
pos <- 0

for (i in 1:10){
        for (j in 1:length(ConfBase)) {
                Conf  <- ConfBase[j]
                ModeloITE <-  Entrenamiento(Subs.Train.V3[1:15],as.factor(Subs.Train.V3$Churn), Conf,i )            #GENERAR MODELO
                PredITE <- predict(ModeloITE, Subs.Test.V3)
                
                matrizConf <- confusionMatrix(data = PredITE, reference = as.factor(Subs.Test.V3$Churn), positive = "True.")
                
                Datos <- c(ModeloITE$control$CF,ModeloITE$control$minCases,
                           ModeloITE$size,matrizConf$byClass,matrizConf$overall )                                                      
                Resultadosv3 <- rbind(Resultadosv3,Datos)   
                pos <- pos + 1
        }
}

Resultadosv3 <- Resultadosv3[-c(1),]  #ELIMINO PRIMERA FILA QUE POR EL RBIND SE COLOCO


head(Resultadosv3)

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


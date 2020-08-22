library (C50)
library(foreign) 

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

## TRAINING
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

# Chequea igual cant de columnas
dim(Sub_Test_EST1)[2] - dim(Sub_Train_EST1)[2]

# Para la realizacion del primer modelo se va proceder a utilizar TODAS las variables 
Modelo1 <- Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, 0.25, 2)

summary(Modelo1)

plot(Modelo1)
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
# 

Informacion <- data.frame("CONF","SIZE TREE","ACCURACY","M")
ConfBase <- 0.05
for (i in 1:5){
        for (j in 0.05:0.95) {
              Conf  <- ConfBase*j
              Modelo <- Entrenamiento(Sub_Train_EST1[1:15],Sub_Train_EST1$Churn, Conf,i )
              
        }
}




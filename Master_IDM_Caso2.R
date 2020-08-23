
# PAQUETES NECESARIOS.
library(readxl)         #Lectura de archivos
library(gridExtra)      #Ordenacion de graficos en pantalla
library(corrplot)       #Grafico de correlaciones
library(ggplot2)        #Graficos
library (tidyverse)     #Manipulacion de datasets
library(vcd)            #Visualizacion de datos categoricos
library(GGally)         #Extension de ggplot2 para combinacion de graficos/matrices
library(MASS)           #Utilizado para la estimacion del mve en distancias de Mahalanobis

# Limpieza de entorno
rm(list = ls())

# Recordar setwd al path correspondiente

# Lectura del archivo Excel mediante ventana emergente
subscriptores.raw <- read.csv2(file.choose())


# Verificacion de las variables y su tipo
str(subscriptores.raw)
summary(subscriptores.raw)
# La variable Area_Code se cargo como numerica pero en realidad es una variable categorica.
# Utilizamos la funcion mutate para convertirla al tipo de dato factor
# subscriptores.raw <- mutate(subscriptores.raw, Area_Code = as.factor(Area_Code))


#####HK Duda:Convierto las 4 categoricas en Factor (estaba en el HTML, pero no lo encontre en el cod R) ####

names <- c('State','Phone', 'Area_Code','Intl_Plan','Vmail_Plan','Churn')
subscriptores.raw[,names] <- lapply(subscriptores.raw[,names] , factor)
str(subscriptores.raw)

####HK Duda:convendria que cust serv sea categorica para algun modelo? (es similar a la discusion de #rooms. depende del modelo a utilizar?)####

# subscriptores.raw <- mutate(subscriptores.raw, CustServ_Calls = as.factor(CustServ_Calls))
# Exploracion inicial del dataset y verificacion de valores faltantes (missings)
# No se observan valores faltantes
summary(subscriptores.raw)
head(subscriptores.raw, 10)

# Graficamos las variables. Utilizamos barplots para las categoricas e histogramas para las
# cuantitativas
# NOTA: no graficamos la variable Phone dado que es una variable de identificacion (ID)

# Creacion de vector con los indices de las variables cuantitativas y cualitativas
columnas.num <- which(sapply(subscriptores.raw, class) %in% c("numeric","integer"))
columnas.cat <- which(sapply(subscriptores.raw, class) %in% "factor")

# Debemos borrar del vector de indices de variables cualitativas al indice correspondiente a la 
# variable Phone, para lo cual mediante la funcion grep obtenemos el indice de la columna Phone
borrar <- grep("Phone", colnames(subscriptores.raw))

# Mediante la funcion setdiff eliminanos el indice correspondiente a Phone del vector columnas.cat
columnas.cat <- setdiff(columnas.cat, borrar)

# Inicializamos los vectores en los cuales se guardaran los histogramas (variables cuantitativas) y
# los barplots (variables cualitativas)
histograma <- vector(mode = "list", length = length(columnas.num))
barplt <- vector(mode = "list", length = length(columnas.cat))

# Mediante un loops for creamos los histogramas y barplots los guardamos en los vectores
# correspondientes
for (i in 1:length(columnas.num)){
        histograma[[i]] <- ggplot(subscriptores.raw, aes_string(colnames(subscriptores.raw[columnas.num[i]]))) + 
                geom_histogram(bins = 30, colour = "#1F3552", fill = "#4271AE") + 
                ylab("Cantidad") + xlab(colnames(subscriptores.raw[columnas.num[i]]))
}

for (i in 1:length(columnas.cat)){
        barplt[[i]] <- ggplot(subscriptores.raw, aes_string(colnames(subscriptores.raw[columnas.cat[i]]))) + 
                geom_bar(colour = "#1F3552", fill = "#4271AE") + 
                ylab("Cantidad") + xlab(colnames(subscriptores.raw[columnas.cat[i]]))
}


# Graficamos los histogramas
do.call(grid.arrange, c(histograma, ncol=3))

# Graficamos los barplots
do.call(grid.arrange, c(barplt, ncol=3))



# boxplt <- vector(mode = "list", length = length(columnas.num))
# for (i in 1:length(columnas.num)){
#     boxplt[[i]] <- ggplot(subscriptores.raw, aes_string(colnames(subscriptores.raw[columnas.num[i]]))) +
#         geom_boxplot(colour = "#1F3552", fill = "#4271AE", outlier.colour = "red", width=1) +
#         ylab("") + xlab(colnames(subscriptores.raw[columnas.num[i]]))
# }
# for (i in 1:length(columnas.num)){
#     grid.arrange(histograma[[i]], boxplt[[i]], ncol=2)
# }
# do.call(grid.arrange, c(boxplt, ncol=3))



# Graficos bivariados para variables cuantitativas - Matriz de dispersion
# En el grafico de dispersion no se aprecian correctamente los nombres de las variables. El orden
# de las variables en el grafico es el siguiende (de izquierda a derecha y de arriba hacia abajo)
colnames(subscriptores.raw[columnas.num])

ggpairs(subscriptores.raw[columnas.num], 
        title="Matriz de dispersion",
        upper = list(continuous = wrap("points", alpha = 0.8, color="#4271AE")),
        lower = list(continuous = wrap("points", alpha = 0.8, color="#4271AE")))


# Graficos multivariados para variables cualitativas
# Observaciones sobre el grafico
# 1. La proporcion de subscriptores que no tienen Plan de Voice Mail es mayor, en linea con lo
# observado en los barplots
# 2. La cantidad de subscriptores que no tienen Plan Internacional es mayor independientemente de
# si tienen o no Plan de Voicemail.
# 3. La proporcion de churn es menor entre los subscripores SIN Voice Mail y SIN Plan Internacional en
# comparacion con los subscriptores SIN Voice Mail y CON Plan Internacional donde la proporcion de
# churn y no churn son mas parejas
# 4. La proporcion de churn es menor entre los subscripores CON Voice Mail y SIN Plan Internacional en
# comparacion con los subscriptores CON Voice Mail y CON Plan Internacional donde la proporcion de
# churn y no churn son mas parejas
# 5. Por los puntos 3 y 4 las proporciones de churn y no churn son mas similares en subscriptores con
# Planes internacionales, independientemente de si tienen o no Plan de Voicemal.
doubledecker(Area_Code ~ Vmail_Plan + Intl_Plan + Churn, data = subscriptores.raw,
             gp = gpar(fill = c("Green", "Red", "Blue")))

# doubledecker(Area_Code ~ Vmail_Plan + Churn, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red", "Blue")))
# 
# doubledecker(Area_Code ~ Intl_Plan + Churn, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red", "Blue")))
# 
# doubledecker(Churn ~ Area_Code, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red")))
# doubledecker(Churn ~ Area_Code + Vmail_Plan + Intl_Plan, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red")))
# doubledecker(Area_Code ~ Churn + Vmail_Plan + Intl_Plan, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red", "Blue")))
# 
# doubledecker(Area_Code ~ Churn + Intl_Plan+ Vmail_Plan, data = subscriptores,
#              gp = gpar(fill = c("Green", "Red", "Blue")))



# Matriz de correlacion - Variables cuantitativas
# Se observa que las variables correlacionadas (positivamente y con valor 1) son
# Day_Mins y Day_Charge
# Eve_Mins y Eve_Charge
# Night_Mins y Night_Charge
# Intl_Mins y Intl_Charge
# Esto indica que a mayor cantidad de minutos consumidos, mayor el cargo.

corrplot(cor(subscriptores.raw[columnas.num]), 
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         number.cex=0.6,
         addCoef.col = "black")

# De entre las 8 varaibales correlacionadas se pueden eliminar 4 variables. Las variables 
# seleccionadas para eliminacion son las correspondientes a los cargos: Day_Charge, Eve_Charge, 
# Night_Charge, Intl_Charge.
# Creamos un nuevo dataset reducido en 4 columas: subscriptores.red
subscriptores.red <- subscriptores.raw[, !colnames(subscriptores.raw) %in% c("Day_Charge", "Eve_Charge", "Night_Charge", "Intl_Charge")]

#### 1.Genero el Dataset1 (DS1):Base total sin Charges que estaban correlacionadas con los minutos #######
write.csv(subscriptores.red,'DS1.csv')

# Tras eliminar las 4 varaibles cuantitativas edefinimos el vector con los indices de las 
# variables cuantitativas
columnas.num <- which(sapply(subscriptores.red, class) %in% c("numeric","integer"))

# Matriz de dispersion tras eliminas las columnas correlacionadas
ggpairs(subscriptores.red[columnas.num], 
        title="Matriz de dispersion",
        upper = list(continuous = wrap("points", alpha = 0.8, color="#4271AE")),
        lower = list(continuous = wrap("points", alpha = 0.8, color="#4271AE")))

# OUTLIERS
# Utilizamos distancias de Mahalanobis para el analisis de outliers de manera multivariada.
# Utilizamos el elipsoide de volumen minimo dado que es considerado un estimador robusto de posicion
# y escala para datos multivariados
cov.subs <- cov.rob(subscriptores.red[columnas.num], method = "mve", nsamp = "best")

dmahal <- mahalanobis(subscriptores.red[columnas.num], cov.subs$center, cov.subs$cov, inverted = F)

summary(dmahal)

# Estimo el valor para el cual por encima del mismo debería considerar que la observacion es un 
# outlier (las distancias de Mahalanobis siguen una distribucion chi cuadrado y tomo p = 0.05 con 
# grados de libertad igual al numero de variables cuantitativas menos 1 (la cantidad de variables 
# cuantitativas esta dado por el largo del vector columnas.num, al cual le restamos 1 para obtener
# los grados de libertad). Este valor se puede tomar de una tabla chi2 o estimarlo a traves de R:
valorCorte <- qchisq(0.95, length(columnas.num) - 1)

# Adjuntamos las distancias de mahalanobis al dataset
subscriptores.red$Mahalanobis <- dmahal

# Adjuntamos una columna al dataset para indicar si es oultlier o no basado en las distnacias de 
# Mahalanobis. Va a ser una columna con valores TRUE o FALSE. Inicializo primero todos los valores 
# en FALSE
subscriptores.red$Outlier <- FALSE
subscriptores.red$Outlier[subscriptores.red$Mahalanobis > valorCorte] <- TRUE

# Aplicamos una suma sobre la columna Outlier (los valores TRUE cuentan como 1) para obtener la
# cantidad de outliers detectados segun las distancias de Mahalanobis
sum(subscriptores.red$Outlier)

# Hago una matriz de dispersion indicando en rojo las observaciones que son
# outliers segun las distnacias de Mahalanobis
ggpairs(subscriptores.red[columnas.num], aes(color=subscriptores.red$Outlier),
        title="Outliers en rojo",
        upper = list(continuous = wrap("points", alpha = 0.3)),
        lower = list(continuous = wrap("points", alpha = 0.3))) +
        scale_fill_manual(values=c("green3", "red2")) +
        scale_color_manual(values=c("green3", "red2"))


# Creamos 2 nuevos dataset. En uno colocamos los outliers (subscriptores.out) y en el otro los 
# registros depurados de outliers (subscriptores.dep)
subscriptores.out <- subscriptores.red[subscriptores.red$Outlier == TRUE,] 
subscriptores.dep <- subscriptores.red[subscriptores.red$Outlier == FALSE,] 


#### 2.Genero el Dataset2 (DS2):Idem DS1, quitandole outliers por mahalanobis #######
write.csv(subscriptores.dep,'DS2.csv')


# Dividimos en el dataset depurado (sin outliers) en 2 datasets, uno de test y otro de pruebas
# Para cada registro generamos un numero aleatorio basado en una distribucion uniforme y lo 
# guardamos como una nueva columna en el dataset.
set.seed(123)
subscriptores.dep$Test_Split <- runif(dim(subscriptores.dep)[1])

# Basados en el numero aleatorio asignado a cada registro dividimos el dataset depurado en training
# y tests. En las siguientes lineas se asigna aproximadamente un 10% a test y aproximadamente un 90%
# a training
subscriptores.test <- subscriptores.dep[subscriptores.dep$Test_Split <= 0.1,]
subscriptores.training <- subscriptores.dep[subscriptores.dep$Test_Split > 0.1,]

View(subscriptores.test)
dim(subscriptores.training)



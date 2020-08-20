getwd()
setwd("/Users/gabyherediabaek/Desktop/DM/IDM/caso 2")

library(gridExtra)      #Ordenacion de graficos en pantalla
library(corrplot)       #Grafico de correlaciones
library(ggplot2)        #Graficos
library (tidyverse)     #Manipulacion de datasets
library(vcd)            #Visualizacion de datos categoricos
library(GGally)         #Extension de ggplot2 para combinacion de graficos/matrices
library(MASS)  

subscriptores.raw <- Churn

# Verficacion de las variables y su tipo
str(subscriptores.raw)
head(subscriptores.raw, 10)


# Graficamos las variables. Utilizamos barplots para las categoricas e histogramas para las
# cuantitativas

# NOTA: no graficamos la variable Phone dado que es una variable de identificacion (ID)



### GABY ####

### para el que no le haya aparecido intl_charge como numeric (por la coma)

subscriptores.raw$Intl_Charge <- as.factor(subscriptores.raw$Intl_Charge)

subscriptores.raw$Intl_Charge <- as.numeric(sub(",", ".", subscriptores.raw$Intl_Charge, fixed = TRUE))
subscriptores.raw$Intl_Charge <- round(subscriptores.raw$Intl_Charge)

### fin gaby ###


# Creacion de vector con los indices de las variables cuantitativas y cualitativas

columnas.num <- which(sapply(subscriptores.raw, class) %in% c("numeric","integer"))

#### GABY ####

### PARA EL QUE NO HAYA PODIDO CORRER (COMO YO) quizas convertir a factor les deja

colnames(subscriptores.raw)
# col <- c(1,3,5,6,21)

subscriptores.raw$State <- as.factor(subscriptores.raw$State)
subscriptores.raw$Area_Code <- as.factor(subscriptores.raw$Area_Code)
subscriptores.raw$Intl_Plan <- as.factor(subscriptores.raw$Intl_Plan)
subscriptores.raw$Vmail_Plan <- as.factor(subscriptores.raw$Vmail_Plan)
subscriptores.raw$Churn <- as.factor(subscriptores.raw$Churn)

### Fin del comunicado ###

columnas.cat <- which(sapply(subscriptores.raw, class) %in% "factor")

# Debemos borrar del vector de indices de variables cualitativas al indice correspondiente a la 
# variable Phone, para lo cual mediante la funcion grep obtenemos el indice de la columna Phone
#borrar <- grep("Phone", colnames(subscriptores.raw))
# Mediante la funcion setdiff eliminanos el indice correspondiente a Phone del vector columnas.cat
#columnas.cat <- setdiff(columnas.cat, borrar)

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


# which(is.na(columnas.cat))

# Graficamos los histogramas
do.call(grid.arrange, c(histograma, ncol=3))

# Graficamos los barplots
do.call(grid.arrange, c(barplt, ncol=3))


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

colnames(subscriptores.raw)

# doubledecker(Area_Code ~ Vmail_Plan + Churn, data = subscriptores.raw,
         #    gp = gpar(fill = c("Green", "Red", "Blue")))

# doubledecker(Area_Code ~ Intl_Plan + Churn, data = subscriptores.raw,
         #    gp = gpar(fill = c("Green", "Red", "Blue")))

doubledecker(Churn ~ Area_Code, data = subscriptores.raw,
         #    gp = gpar(fill = c("Green", "Red")))

# doubledecker(Churn ~ State + Day_Mins, data = subscriptores.raw,
         #    gp = gpar(fill = c("Green", "Red")))

# doubledecker(Area_Code ~ Churn + Vmail_Plan + Intl_Plan, data = subscriptores.raw,
         #    gp = gpar(fill = c("Green", "Red", "Blue")))

# doubledecker(Area_Code ~ Churn + Intl_Plan+ Vmail_Plan, data = subscriptores.raw,
        #    gp = gpar(fill = c("Green", "Red", "Blue")))


##### GABY #####

## VARIABLE AGREGADA

subscriptores.raw$Ambos_planes <- as.character(subscriptores.raw$Intl_Plan) == as.character(subscriptores.raw$Vmail_Plan)

## VARIABLE MAS DE 4 LLAMADAS

subscriptores.raw$CustServ_Calls <- as.factor(subscriptores.raw$CustServ_Calls)

levels(subscriptores.raw$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")
subscriptores.raw$Customer_service <- ifelse(subscriptores.raw$CustServ_Calls %in% normales, "<4", "=>4")
# table(subscriptores.raw$Customer_service)

## VARIABLE TOTAL CHARGE

subscriptores.raw$Total_Charge <- (subscriptores.raw$Day_Charge) + (subscriptores.raw$Eve_Charge)
subscriptores.raw$Total_Charge <- (subscriptores.raw$Total_Charge) + (subscriptores.raw$Night_Charge)
subscriptores.raw$Total_Charge <- (subscriptores.raw$Total_Charge) + as.numeric(subscriptores.raw$Intl_Charge)
subscriptores.raw$Total_Charge <- round(subscriptores.raw$Total_Charge)


### GRAFICOS DE PROPORCIONES

## library(reshape2) (se puede subir a la lista)

# Plan Internacional

my_table <- table(subscriptores.raw$Intl_Plan, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)
my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$International_Plan <- rownames(my_table_3)
long_data <- melt(my_table_3, id.vars= c("International_Plan"),
                  value.name="Proportion")

names(long_data)[2] <- paste("Churn")

ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= International_Plan),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and International Plan") +
                     scale_fill_brewer(palette = "Paired")

# Voice Mail # no parece tan significativa esta diferencia, pero los Churn tienen VMail_plan en menor proporcion

my_table <- table(subscriptores.raw$Vmail_Plan, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)
my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$Voice_Mail_Plan <- rownames(my_table_3)
long_data <- melt(my_table_3, id.vars= c("Voice_Mail_Plan"), value.name="Proportion")
names(long_data)[2] <- paste("Churn")

ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= Voice_Mail_Plan),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and Voice Mail") +
                     scale_fill_brewer(palette = "Paired")

## Customer service > 4

my_table <- table(subscriptores.raw$Customer_service, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)
my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$Customer_Service <- rownames(my_table_3)
long_data <- melt(my_table_3, id.vars= c("Customer_Service"), value.name="Proportion")
names(long_data)[2] <- paste("Churn")
ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= Customer_Service),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and Customer Service") +
                     scale_fill_brewer(palette = "Paired")



## Histogramas

# Para cantidad de mensajes de voz... bastante irrelevante, solo dice que no hay grandes diferencias para esta variable

# colnames(subscriptores.raw)
# subscriptores.raw$Area_Code <- as.factor(subscriptores.raw$Area_Code)

library(ggridges) ## se puede agregar arriba

ggplot(subscriptores.raw, aes(x = Vmail_Message, y = Churn, fill = Churn)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

colnames(subscriptores.raw)

# para antiguedad (Acct length) ... bastante irrelevante, solo dice que no hay grandes diferencias para esta variable

ggplot(subscriptores.raw, aes(x = Account_Length, y = Churn, fill = Churn)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

# minutos diurnos

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")
col <- c(8, 21)
No_Churn_1 <- No_Churn[,col]
Churn_1 <- Churn[,col]


p1 <- hist(No_Churn_1$Day_Mins, plot = F)                   
p2 <- hist(Churn_1$Day_Mins, plot = F)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,3500))
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,3500), add=T)

## el que les parezca mas claro 

## ggplot(subscriptores.raw, aes(x = Day_Mins, y = Churn, fill = Churn)) +
##  geom_density_ridges() +
##  theme_ridges() + 
##  theme(legend.position = "none") +
##  scale_fill_brewer(palette = "Blues")

# minutos tarde (no aporta)

col <- c(11, 21)
No_Churn_2 <- No_Churn[,col]
Churn_2 <- Churn[,col]

p1 <- hist(No_Churn$Eve_Mins, plot = F)                   
p2 <- hist(Churn$Eve_Mins, plot = F)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,4000)) 
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,4000), add=T)

# minutos noche (tampoco)

# col <- c(14, 21)
# No_Churn_3 <- No_Churn[,col]
# Churn_3 <- Churn[,col]
# 
# p1 <- hist(No_Churn$Night_Mins, plot = F)                   
# p2 <- hist(Churn$Night_Mins, plot = F)                    
# plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,4000))  
# plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,4000), add=T)

# acct lenght (tampoco)

col <- c(2, 21)
No_Churn_4 <- No_Churn[,col]
Churn_4 <- Churn[,col]
p1 <- hist(No_Churn$Account_Length, plot = F)                   
p2 <- hist(Churn$Account_Length,plot = F)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,300))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,300), add=T)

# international mins

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")
col <- c(17, 21)
No_Churn_5 <- No_Churn[,col]
Churn_5 <- Churn[,col]

p1 <- hist(No_Churn$Intl_Mins, plot = F)                   
p2 <- hist(Churn$Intl_Mins, plot = F)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,200))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,200), add=T)

### me parecen mas intuitivos los otros pero la que sigue es una opcion

ggplot(subscriptores.raw, aes(x = Intl_Mins, y = Churn, fill = Churn)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

### total_charge: parece un poco relevante

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")
col <- c(24, 21)
No_Churn_6 <- No_Churn[,col]
Churn_6 <- Churn[,col]

p1 <- hist(No_Churn$Total_Charge, plot = F)                 
p2 <- hist(Churn$Total_Charge, plot = F)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,12000))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,12000), add=T)

## cualquier opcion que les parezca... la segunda me parece mas "impactante"

ggplot(subscriptores.raw, aes(x = Total_Charge, y = Churn, fill = Churn)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Blues")

### Los churners hablan mas de dia y llaman mas a At. al cliente

ggplot(data = subscriptores.raw, aes(Day_Mins, CustServ_Calls, color = Churn)) +
  geom_point() +
  scale_color_manual(values = c("False." = "blue2", "True." = "red2"))


# BOXPLOTS con codigos de area

## (no se si son interesantes... tampoco entiendo muy bien como cada estado tiene los
## 3 codigos de area...)

subscriptores.raw$Area_Code <- as.factor(subscriptores.raw$Area_Code)

ggplot(subscriptores.raw, aes(x=Churn, y=Intl_Mins, fill= Area_Code)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=10, size=3) +
    ggtitle("Churn por Codigo de area y minutos internacionales") +
    xlab("Churn") +
    ylab("International Minutes") +
    scale_fill_brewer(palette = "Blues")

ggplot(subscriptores.raw, aes(x=Churn, y=Account_Length, fill= Area_Code)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=10, size=3) +
    ggtitle("Churn por Codigo de area y Antiguedad") +
    xlab("Churn") +
    ylab("Account Length") +
    scale_fill_brewer(palette = "Blues")

## FIN GABY ### 


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

subscriptores.red <- subscriptores.raw[, !colnames(subscriptores.raw) %in% c("Day_Charge", 
                                                                             "Eve_Charge", "Night_Charge", "Intl_Charge")]

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

447/2652
88/283
359/2369

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


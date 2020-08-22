library(mice) 
library(ggplot2) 
library(gridExtra)

# choose para elegir el archivo de datos
winDialog("okcancel","Seleccionar el archivo de datos")
datos <- read.csv2(file.choose(),)
summary(datos)


###################################
###### PUNTO 2
###################################
# Código de Area es numérica pero categorica. Factorizar
datos$Area_Code <- as.factor(datos$Area_Code)
levels(datos$Area_Code)
summary(datos)

# Revisamos código de área con states y vemos que no tiene sentido
# cada state está en los 3 códigos de área
ggplot(datos, aes(x=State, y = Area_Code, color=State)) +
  geom_jitter() +
  theme(legend.position="none")


###################################
###### PUNTO 4
###################################
# Cantidad de llamadas a Customer Service
# Analizamos las frecuencias
hist(datos$CustServ_Calls, main="", xlab="Customer Service Calls", col="blue", breaks=10)
summary(datos$CustServ_Calls)
sqrt(var(datos$CustServ_Calls))
# Analizamos los cuartiles 
ggplot(datos, aes(y=CustServ_Calls)) +
  geom_boxplot() 

# Puntuación Z (Nos dá para valores de Call >= a 6)
VZ <- data.frame(calls = datos$CustServ_Calls,  
                 z = ((datos$CustServ_Calls - mean(datos$CustServ_Calls)) / sqrt(var(datos$CustServ_Calls))))
ggplot(VZ, aes(y=calls, x=z)) +
  geom_boxplot() 

# Por RIC
# Dado que la variable no tiene valores negativos
# consideramos el límite superior que resulta en 3
# Cualquier valor superior a 3 sería considerado unposible outlier
summary(datos$CustServ_Calls)
Q1 <- 1
Q3 <- 2
limite1 <- Q1 * -1.5
limite2 <- Q3 * 1.5
hist(datos$CustServ_Calls, main="", xlab="Customer Service Calls", col="blue", breaks=10)
abline(v=limite2, lwd=2, lty=3, col="red") 


###################################
###### PUNTO 5
###################################
# Se transforma la variable Day Minutes con puntaje z
# Se observa una distribución simétrica 
z <- ((datos$Day_Mins - mean(datos$Day_Mins)) / sqrt(var(datos$Day_Mins)))
datos$Day_Mins_T <- z
ggplot(datos, aes(Day_Mins_T)) +
  geom_histogram(col="lightblue") 

###################################
###### PUNTO 6
###################################
#Sesgo variable day minute
# En primera instancia sobre la variable no transformada
# se observa una distribución bastante simétrica
# lo mismo ocurre con la variable Transformada dado que 
# la distribución no cambia al ser transformada por puntaje z
summary(datos$Day_Mins)
sesgo <- (3 * (179.6 - 179.3)) / sqrt(var(datos$Day_Mins))
ggplot(datos, aes(Day_Mins)) +
  geom_histogram(col="lightblue", fill="cyan") +
  geom_freqpoly()

summary(datos$Day_Mins_T)
sesgo_T <- (3 * (0 + 0.005436)) / sqrt(var(datos$Day_Mins_T))
ggplot(datos, aes(Day_Mins_T)) +
  geom_histogram(col="pink", fill="purple") +
  geom_freqpoly()

if (round(sesgo,digits=2) == round(sesgo_T,digits=2)) print("Son iguales")

###################################
###### PUNTO 7
###################################
# Normal Probability Plot de Day Minute
qqnorm(datos$Day_Mins, col="green", 
       main="Normal Prob Plot Day Minute")
qqline(datos$Day_Mins, col="red")

###################################
###### PUNTO 8
###################################
# subpunto a: Normal Probability Plot de International Minute
qqnorm(datos$Intl_Mins, col="lightblue", 
       main="Normal Prob Plot International Minute")
qqline(datos$Intl_Mins, col="red")

# subpunto b: distribución de la variable
# se calcula el sesgo
# Se obtiene un Sesgo Negativo y esto impide la simetría que requiere
# una distribución Normal
summary(datos$Intl_Mins)
sesgo <- (3 * (10.24 - 10.30)) / sqrt(var(datos$Intl_Mins))

ggplot(datos, aes(Intl_Mins)) +
  geom_histogram(col="blue", fill="lightblue") +
  geom_freqpoly()

# subpunto c: Transformación
# Se calculan variables nuevas de transformación para 
# seleccionar la que mejor acerca a la variable a una 
# distribución normal - Se elige la técnica Scale

# Con SQRT
t_sqrt<- sqrt(datos$Intl_Mins)
datos$Intl_Mins_T_SQRT <- t_sqrt
summary(datos$Intl_Mins_T_SQRT)
plot1 <- ggplot(datos, aes(Intl_Mins_T_SQRT)) +
  geom_histogram(col="blue", fill="lightblue") +
  geom_freqpoly()

# Con scale
t_scl<- scale(datos$Intl_Mins)
datos$Intl_Mins_T_SCL <- t_scl
summary(datos$Intl_Mins_T_SCL)
plot2 <- ggplot(datos, aes(Intl_Mins_T_SCL)) +
  geom_histogram(col="blue", fill="lightblue") +
  geom_freqpoly()

# Con Min-Max
t_mm<- (datos$Intl_Mins - min(datos$Intl_Mins)) / (max(datos$Intl_Mins)- min(datos$Intl_Mins))
datos$Intl_Mins_T_MM <- t_mm
summary(datos$Intl_Mins_T_MM)
plot3<- ggplot(datos, aes(Intl_Mins_T_MM)) +
  geom_histogram(col="blue", fill="lightblue") +
  geom_freqpoly()

plot0<- ggplot(datos, aes(Intl_Mins)) +
  geom_histogram(col="blue", fill="lightblue") +
  geom_freqpoly()
grid.arrange(plot0, plot1, plot2, plot3, ncol=4)

# subpunto d: Normal Prob Plot
# Normal Probability Plot de International Minute
# Podemos comprobar que no cambia respecto a la variable original
qqnorm(datos$Intl_Mins_T_SCL, col="green", 
       main="Normal Prob Plot International Minute Transformada")
qqline(datos$Intl_Mins_T_SCL, col="red")


###################################
###### PUNTO 9
###################################
# Se transforma la variable Night Minutes con puntaje z
z <- ((datos$Night_Mins - mean(datos$Night_Mins)) / sqrt(var(datos$Night_Mins)))
datos$Night_Mins_T <- z
ggplot(datos, aes(Night_Mins_T)) +
  geom_histogram(col="lightblue", fill="beige") +
  geom_freqpoly()

summary(datos$Night_Mins_T)

ggplot(datos, aes(Night_Mins_T)) +
  geom_boxplot(col="green")
                 
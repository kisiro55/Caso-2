
## VARIABLE AGREGADA
subscriptores.raw$Ambos_planes <- as.character(subscriptores.raw$Intl_Plan) == as.character(subscriptores.raw$Vmail_Plan)

## VARIABLE MAS DE 3 LLAMADAS
subscriptores.raw$CustServ_Calls <- as.factor(subscriptores.raw$CustServ_Calls)

levels(subscriptores.raw$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

subscriptores.raw$Customer_service <- ifelse(subscriptores.raw$CustServ_Calls %in% normales, "<4", "=>4")
table(subscriptores.raw$Customer_service)

#######HK: Quizas se podrian sumar una categorica ordinal por llamadas telefonicas?

### GRAFICO DE PROPORCIONES ###

## library(reshape2)

# Plan Internacional

my_table <- table(subscriptores.raw$Intl_Plan, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)
my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$International_Plan <- rownames(my_table_3)

long_data <- melt(my_table_3, id.vars= c("International_Plan"),
                  value.name="Proportion")

names(long_data)[2] <- paste("Churn")

a <- ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= International_Plan),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and International Plan") +
                     scale_fill_brewer(palette = "Paired")
a

# Voice Mail Plan 

my_table <- table(subscriptores.raw$Vmail_Plan, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)

my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$Voice_Mail_Plan <- rownames(my_table_3)

long_data <- melt(my_table_3, id.vars= c("Voice_Mail_Plan"),
                  value.name="Proportion")

names(long_data)[2] <- paste("Churn")

b <- ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= Voice_Mail_Plan),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and Voice Mail") +
                     scale_fill_brewer(palette = "Paired")

# Customer Service

my_table <- table(subscriptores.raw$Customer_service, subscriptores.raw$Churn)
my_table2 <- prop.table(my_table, 2)

my_table_3 <- as.data.frame.matrix(my_table2)
my_table_3$Customer_Service <- rownames(my_table_3)

long_data <- melt(my_table_3, id.vars= c("Customer_Service"),
                  value.name="Proportion")

names(long_data)[2] <- paste("Churn")

c <- ggplot() + geom_bar (aes(y= Proportion,
                     x= Churn,
                     fill= Customer_Service),
                     data = long_data,
                     stat = "identity", col="black")+
                     ggtitle("Churn and Customer Service") +
                     scale_fill_brewer(palette = "Dark2")

# s <- do.call(grid.arrange, c(histograma, ncol=3))

## Histogramas

# minutos diurnos

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")

colnames(No_Churn)

col <- c(8, 21)
No_Churn_1 <- No_Churn[,col]
Churn_1 <- Churn[,col]

p1 <- hist(No_Churn_1$Day_Mins)                   
p2 <- hist(Churn_1$Day_Mins)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,4000), add=T)

# minutos tarde

col <- c(11, 21)
No_Churn_2 <- No_Churn[,col]
Churn_2 <- Churn[,col]

p1 <- hist(No_Churn$Eve_Mins)                   
p2 <- hist(Churn$Eve_Mins)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,4000), add=T)

# minutos noche

col <- c(14, 21)
No_Churn_3 <- No_Churn[,col]
Churn_3 <- Churn[,col]

p1 <- hist(No_Churn$Night_Mins)                   
p2 <- hist(Churn$Night_Mins)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,4000), add=T)

# acct lenght

col <- c(2, 21)
No_Churn_4 <- No_Churn[,col]
Churn_4 <- Churn[,col]

p1 <- hist(No_Churn$Account_Length)                   
p2 <- hist(Churn$Account_Length)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,300))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,300), add=T)

# international mins

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")

subscriptores.raw$Intl_Charge <- as.numeric(sub(",", ".", subscriptores.raw$Intl_Charge, fixed = TRUE))
round(subscriptores.raw$Intl_Charge)

col <- c(17, 21)
No_Churn_5 <- [,col]
Churn_5 <- Churn[,col]

p1 <- hist(No_Churn$Intl_Mins)                   
p2 <- hist(Churn$Intl_Mins)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,200))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,200), add=T)

### total_charge

subscriptores.raw$Total_Charge <- (subscriptores.raw$Day_Charge) + (subscriptores.raw$Eve_Charge)
subscriptores.raw$Total_Charge <- (subscriptores.raw$Total_Charge) + (subscriptores.raw$Night_Charge)
subscriptores.raw$Total_Charge <- (subscriptores.raw$Total_Charge) + (subscriptores.raw$Intl_Charge)
subscriptores.raw$Total_Charge <- round(subscriptores.raw$Total_Charge)

No_Churn <- subset(subscriptores.raw, subset= Churn == "False.")
Churn <- subset(subscriptores.raw, subset= Churn == "True.")

col <- c(24, 21)
No_Churn_6 <- [,col]
Churn_6 <- Churn[,col]

p1 <- hist(No_Churn$Total_Charge)                 
p2 <- hist(Churn$Total_Charge)                    
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,12000))  
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,12000), add=T)




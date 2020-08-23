# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth', 'RWeka'))

library(caret)
library(RWeka)
library (C50)
library(foreign)

# Useful Links
## https://stackoverflow.com/questions/40752070/issues-installing-rweka
## https://www.machinelearningplus.com/machine-learning/caret-package/ ##


#### 3 Data Preparation and Preprocessing ####

#### 3.0 Preparo el DS ####

# LEO LOS DATOS DE LA FASE DE PREPROCESAMIENTO

DS1 <- read.csv('DS1.csv', stringsAsFactors=TRUE) # subscr_test_v1.csv
DS2 <- read.csv('DS2.csv', stringsAsFactors=TRUE) #subscr_training_v1.csv'

# Elimino la primer columna de id y el phone

DS1 <- DS1[-c(1,5)]
DS2 <- DS2[-c(1,5,19,20)]

# Agrego Variable Ambos Planes
DS1$Ambos_planes <- ifelse(as.character(DS1$Intl_Plan) == as.character(DS1$Vmail_Plan), "TRUE", "FALSE")
DS1$Ambos_planes <- as.factor(DS1$Ambos_planes)
# Agrego Variable "Mas de 3 Llamadas"
DS1$CustServ_Calls <- as.factor(DS1$CustServ_Calls)
levels(DS1$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

DS1$Customer_service <- ifelse(DS1$CustServ_Calls %in% normales, "<4", ">=4")
DS1$Customer_service <- as.factor(DS1$Customer_service)

table(DS1$Customer_service)

# Reordeno columnas del dataset para que Churn quede al final
DS1 <- subset(DS1, select=c(State:CustServ_Calls,Customer_service,Ambos_planes,Churn))

#### 3.1 -  How to split the dataset into training and validation? ####

# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(DS1$Churn, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- DS1[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- DS1[-trainRowNumbers,]
dim(trainData)

# Store X and Y for later use.
x = trainData[, 1:17]
y = trainData$Churn

#### Balanceo la muestra ####

# a) opcion downSampledTrain
# set seed and up sample with the following syntax:
set.seed(123)
downSampledTrain <- caret::downSample(x = trainData[, 1:17],
                                  y = trainData$Churn,
                                  yname = 'Churn')

# ensure the classes are now balanced
prop.table(table(downSampledTrain$Churn))

# b) opcion downSampledTrain
set.seed(123)
upSampledTrain <- caret::upSample(x = trainData[, 1:17],
                                      y = trainData$Churn,
                                      yname = 'Churn')

# ensure the classes are now balanced
prop.table(table(upSampledTrain$Churn))


#### '=================' ####
#### 6. Training and Tuning the model ####
#### 6.1. How to train() the model and interpret the results? ####

# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames


# Set the seed for reproducibility
set.seed(100)

# Train the model using Dec Tree and predict on the training data itself.

ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)

####  ENTRENO MODELOS ####

model_C5 = train(Churn ~ ., data=downSampledTrain, method='C5.0', trControl=ctrl)
fitted <- predict(model_C5)
model_C5

model_J48 = train(Churn ~ ., data=downSampledTrain, method='J48', trControl=ctrl)
fitted <- predict(model_C5)
model_J48

model_C5_tree = train(Churn ~ ., data=downSampledTrain, method='C5.0Tree', trControl=ctrl)
fitted <- predict(model_C5_tree)
model_C5_tree

install.packages("randomForest")
library(randomForest)
# model_RF = train(Churn ~ ., data=downSampledTrain, method='rf', trControl=ctrl)

model_RF<- caret::train(Churn ~ ., data = downSampledTrain,method = 'rf', trControl = ctrl )
               
fitted <- predict(model_RF)
model_RF


library(rpart)
model_CART <- rpart(
        Churn ~ ., 
        data = downSampledTrain, 
        method = "class"
)

####  PREDICCION ####

# model_CART = rpart(Churn ~ ., data=downSampledTrain, method='rpart', trControl=ctrl)
fitted <- predict(model_CART)
model_CART

## Training different models
predicted_C5 <- predict(model_C5, testData[,1:17])
head(predicted_C5)

predicted_J48 <- predict(model_J48, testData[,1:17])
head(predicted_J48)

predicted_C5_tree <- predict(model_C5_tree, testData[,1:17])
head(predicted_C5_tree)

predicted_CART <- predict(model_CART, testData[,1:17])
head(predicted_CART)

predicted_RF <- predict(model_RF, testData[,1:17])
head(predicted_RF)

####  CONF MATRIX ####

# Compute the confusion matrix
confusionMatrix(reference = testData$Churn, data = predicted_C5, mode='everything', positive='True.')

confusionMatrix(reference = testData$Churn, data = predicted_J48, mode='everything', positive='True.')

confusionMatrix(reference = testData$Churn, data = predicted_C5_tree, mode='everything', positive='True.')

# confusionMatrix(reference = testData$Churn, data = predicted_CART, mode='everything', positive='True.')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# plot mytree
fancyRpartPlot(model_CART, caption = NULL)

confusionMatrix(reference = testData$Churn, data = predicted_RF, mode='everything', positive='True.')

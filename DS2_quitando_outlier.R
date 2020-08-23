# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

library (C50)
library(foreign)
library(caret)

## https://www.machinelearningplus.com/machine-learning/caret-package/ ##
## LEO LOS DATOS DE LA FASE DE PREPROCESAMIENTO

DS1 <- read.csv('DS1.csv', stringsAsFactors=TRUE) # subscr_test_v1.csv
DS2 <- read.csv('DS2.csv', stringsAsFactors=TRUE) #subscr_training_v1.csv'

# Elimino la primer columna de id y el phone

DS1 <- DS1[-c(1,5)]
DS2 <- DS2[-c(1,5,19,20)]


## Agrego Variable Ambos Planes
DS2$Ambos_planes <- ifelse(as.character(DS2$Intl_Plan) == as.character(DS2$Vmail_Plan), "TRUE", "FALSE")
DS2$Ambos_planes <- as.factor(DS2$Ambos_planes)
## Agrego Variable "Mas de 3 Llamadas"
DS2$CustServ_Calls <- as.factor(DS2$CustServ_Calls)
levels(DS2$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

DS2$Customer_service <- ifelse(DS2$CustServ_Calls %in% normales, "<4", ">=4")
DS2$Customer_service <- as.factor(DS2$Customer_service)

table(DS2$Customer_service)

# Reordeno columnas del dataset para que Churn quede al final
DS2 <- subset(DS2, select=c(State:CustServ_Calls,Customer_service,Ambos_planes,Churn))

#### 3 Data Preparation and Preprocessing ####
#### 3.1 -  How to split the dataset into training and validation? ####

# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(DS2$Churn, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- DS2[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- DS2[-trainRowNumbers,]
dim(trainData)

# Store X and Y for later use.
x = trainData[, 1:17]
y = trainData$Churn

#### 3.2. Descriptive statistics ####
library(skimr)
skimmed <- skim_to_wide(trainData)
skimmed[, c(1:15)]
#### Nota: probar pasar Area_Code a Factor? Entender la logica del areacode vs. States ####

#### '=================' ####
#### 6. Training and Tuning the model ####
#### 6.1. How to train() the model and interpret the results? ####

# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames
modelLookup('C5.0')

# Set the seed for reproducibility
set.seed(100)

# Train the model using Dec Tree and predict on the training data itself.

ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)

# rfFit <- train(Class ~ ., data=Sonar, 
#                method="rf", preProc=c("center", "scale"), 
#                trControl=ctrl)


model_C5 = train(Churn ~ ., data=trainData, method='C5.0', trControl=ctrl)
fitted <- predict(model_C5)
model_C5

plot(model_C5, main="Model Accuracies with C5.0")


#### 6.2 How to compute variable importance? ####
varimp_C5 <- varImp(model_C5)
plot(varimp_C5, main="Variable Importance with C5.0")

#### 6.3. Prepare the test dataset and predict ####
# Step 1: Impute missing values 
# testData2 <- predict(preProcess_missingdata_model, testData)  

# Step 2: Create one-hot encodings (dummy variables)
# testData2 <- predict(dummies_model, testData)

# Step 3: Transform the features to range between 0 and 1
# testData3 <- predict(preProcess_range_model, testData2)

# View
head(testData[, 1:17])

#### 6.4. Predict on testData ####
# Predict on testData
predicted <- predict(model_C5, testData[,1:17])
head(predicted)

#### 6.5. Confusion Matrix ####
# Compute the confusion matrix
confusionMatrix(reference = testData$Churn, data = predicted, mode='everything', positive='True.')


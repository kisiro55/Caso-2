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
modelLookup('J48')

# Set the seed for reproducibility
set.seed(100)

# Train the model using Dec Tree and predict on the training data itself.

ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)

## Training different models

model_C5 = train(Churn ~ ., data=downSampledTrain, method='C5.0', trControl=ctrl)
fitted <- predict(model_C5)
model_C5

model_J48 = train(Churn ~ ., data=downSampledTrain, method='J48', trControl=ctrl)
fitted <- predict(model_C5)
model_J48


## Training different models
predicted_C5 <- predict(model_C5, testData[,1:17])
head(predicted_C5)

predicted_J48 <- predict(model_C5, testData[,1:17])
head(predicted_J48)



#### 6.5. Confusion Matrix ####
# Compute the confusion matrix
confusionMatrix(reference = testData$Churn, data = predicted_C5, mode='everything', positive='True.')

confusionMatrix(reference = testData$Churn, data = predicted_J48, mode='everything', positive='True.')














#### Visualize ROC Curves ####

library(ROCR)
library(pROC)
# Select a parameter setting
selectedIndices <- rfFit$pred$True. == 2
# Plot:
plot.roc(model_C5$pred$obs[selectedIndices],
         model_C5$pred$M[selectedIndices])


#### 7. How to do hyperparameter tuning to optimize the model for better performance? ####

#### 7.1. Setting up the trainControl() ####

# Define the training control
fitControl <- trainControl(
        method = 'cv',                   # k-fold cross validation
        number = 5,                      # number of folds
        savePredictions = 'final',       # saves predictions for optimal tuning parameter
        classProbs = T,                  # should class probabilities be returned
        summaryFunction=twoClassSummary  # results summary function
) 

#### 7.2 Hyper Parameter Tuning using tuneLength ####

# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)
# 
# ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
# grid <- expand.grid(mtry = c(3,4,5))
# 
# model <- train(Churn ~ ., data = trainData, method = "C5.0", tuneGrid = grid, trControl = ctrl, metric="Sens")


model_C5_2 = train(Churn ~ ., data=trainData, method='C5.0', tuneLength = 5, metric='ROC', trControl = fitControl)
model_C5_2

# Step 2: Predict on testData and Compute the confusion matrix
predicted2 <- predict(model_C5_2, testData[,1:17])
confusionMatrix(reference = testData$Churn, data = predicted2, mode='everything', positive='True.')


#### 7.3. Hyper Parameter Tuning using tuneGrid ####

# Step 1: Define the tuneGrid
ConfBase <- seq(0.05, 0.9, by = 0.05)


C50Grid <- expand.grid(trials = c(1:9, (1:10)*10),
                       model = c("tree", "rules"),
                       winnow = c(TRUE, FALSE))

# Step 2: Tune hyper parameters by setting tuneGrid
set.seed(476)
# c50TuneBaseline <- train(x = trainData[1:17],
#                          y = trainData$Churn,
#                          method = 'C5.0',
#                          tuneGrid = c50Grid,
#                          verbose = FALSE,
#                          metric = ROC,
#                          trControl = ctrl)
# c50TuneBaseline

model_C5_3 = train(Churn ~ ., data=trainData, method='C5.0', metric='Sens', tuneGrid = C50Grid, trControl = fitControl)
model_C5_3

# Step 3: Predict on testData and Compute the confusion matrix
predicted3 <- predict(model_C5_3, testData[,1:17])
confusionMatrix(reference = testData$Churn, data = predicted3, mode='everything', positive='True.')


library(pROC)
# create object ValPredProb by predicting validation set
# postive class (gt50) probabilities using C50TrainSens
C50ROC <- pROC::roc(testData$Churn,
                    0.14,
                    levels = rev(levels(testData$Churn)))
#
# determine the new cutoff to be used on the test set
C50Thresh <- pROC::coords(C50ROC, x = best, best.method = closest.topleft)
newPredictions <- factor(ifelse(testPred > C50Thresh[1],
                                gt50, le50))

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
DS1$Ambos_planes <- ifelse(as.character(DS1$Intl_Plan) == as.character(DS1$Vmail_Plan), "TRUE", "FALSE")
DS1$Ambos_planes <- as.factor(DS1$Ambos_planes)
## Agrego Variable "Mas de 3 Llamadas"
DS1$CustServ_Calls <- as.factor(DS1$CustServ_Calls)
levels(DS1$CustServ_Calls)
normales <- c("0", "1", "2", "3")                    
quejosos <- c( "4", "5", "6","7", "8", "9")

DS1$Customer_service <- ifelse(DS1$CustServ_Calls %in% normales, "<4", ">=4")
DS1$Customer_service <- as.factor(DS1$Customer_service)

table(DS1$Customer_service)

# Reordeno columnas del dataset para que Churn quede al final
DS1 <- subset(DS1, select=c(State:CustServ_Calls,Customer_service,Ambos_planes,Churn))

#### 3 Data Preparation and Preprocessing ####
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

#### 3.2. Descriptive statistics ####
library(skimr)
skimmed <- skim_to_wide(trainData)
skimmed[, c(1:15)]
#### Nota: probar pasar Area_Code a Factor? Entender la logica del areacode vs. States ####
#### 3.3. How to impute missing values using preProcess()? ####

# En este caso no hay missing pero de haber habido se podria haber ejecutado el siguiente codigo: 

# Create the knn imputation model on the training data
# preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
# preProcess_missingdata_model

# Use the imputation model to predict the values of missing data points
# library(RANN)  # required for knnInpute
# trainData <- predict(preProcess_missingdata_model, newdata = trainData)
# anyNA(trainData)

#### 3.4. How to create One-Hot Encoding (dummy variables)? ####

str(trainData)
# DudaHK: vale la pena hacer one hot encoding del State? Son 51 levels!!

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.

dummies_model <- dummyVars(Churn ~ ., data=trainData[-c(1)], fullRank=T) # fullRank=T evita colinealidad tomando n-1 columnas para representar las n variables.

#Idem pero con dummies de State
# dummies_model <- dummyVars(Churn ~ ., data=trainData, fullRank=T)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)

# # Convert to dataframe
trainData1 <- data.frame(trainData_mat)

# Append the Y variable
trainData1$State <- trainData[c(1)]
trainData1$Churn <- y

# # See the structure of the new dataset
str(trainData1)
dim(trainData1)

# Probar este codigo para aplicar one hot encoding solo a algunas columnas
        # factor_columns <- names(which(lapply(train, class) == "factor"))
        # factor_predictors <- setdiff(factor_columns, c("sold", "UniqueID"))
        # dummies_formula <- as.formula(paste("~ ", paste0(factor_predictors, collapse=" + ")))
        # dummies <- dummyVars(dummies_formula, data=train, fullRank=TRUE)
        # train_dummies <- as.data.frame(predict(dummies, newdata=train))

#### DudaHK: Es recomendado el one-hot-encoding para decision trees por ejemplo? Se suele hacer encoding ya que se probara con diferentes modelos y en gral requieren este preprocesamiento? Probar con y sin one-hot para ver diferencias en la practica ####

#### 3.5. How to preprocess to transform the data? #### 
# With the missing values handled and the factors one-hot-encoded, our training dataset is now ready to undergo variable transformations if required.
# So what type of preprocessing are available in caret?
# range: Normalize values so it ranges between 0 and 1
# center: Subtract Mean
# scale: Divide by standard deviation
# BoxCox: Remove skewness leading to normality. Values must be > 0
# YeoJohnson: Like BoxCox, but works for negative values.
# expoTrans: Exponential transformation, works for negative values.
# pca: Replace with principal components
# ica: Replace with independent components
# spatialSign: Project the data to a unit circle
#### Try: Probar haciendo feature scaling ####
        # preProcess_range_model <- preProcess(trainData, method= c("scale"))
        # 
        # ####DudaHK: dec trees precisa range o scale? me parece que no es sensible. lo podemos probar ####
        # trainData <- predict(preProcess_range_model, newdata = trainData)
        # 
        # # Append the Y variable
        # trainData$Churn <- y
        # apply(trainData[, 1:15], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

#### '=================' ####
#### 4. How to visualize the importance of variables using featurePlot() ####
# columnas.num <- which(sapply(trainData1, class) %in% c("numeric","integer"))
# columnas.num
# trainData2 <-trainData1[columnas.num]
str(trainData1$State)
summary(trainData1$State)
dim(trainData1$State)
### ! Descomentar ####
        # featurePlot(x = trainData1[, 1:14], 
        #             y = trainData1$Churn,
        #             plot = "box",
        #             strip=strip.custom(par.strip.text=list(cex=.7)),
        #             scales = list(x = list(relation="free"), 
        #                           y = list(relation="free")))
        # 
        # 
        # 
        # featurePlot(x = trainData1[, 1:14], 
        #             y = trainData1$Churn, 
        #             plot = "density",
        #             strip=strip.custom(par.strip.text=list(cex=.7)),
        #             scales = list(x = list(relation="free"), 
        #                           y = list(relation="free")))

#### Conclusion: Se observa la importancia de CustServ_Calls, Day_mins y Intl_plan en la variable a predecir Churn. So to be safe, let’s not arrive at conclusions about excluding variables prematurely.####
#### '=================' ####
#### 5. How to do feature selection using recursive feature elimination (rfe)? ####

        # set.seed(100)
        # options(warn=-1)
        # 
        # subsets <- c(1:4)
        # 
        # ctrl <- rfeControl(functions = rfFuncs,
        #                    method = "repeatedcv",
        #                    repeats = 5,
        #                    verbose = FALSE)
        # 
        # lmProfile <- rfe(x=trainData1[, 1:14], y=trainData$Churn,
        #                  sizes = subsets,
        #                  rfeControl = ctrl)
        # 
        # lmProfile

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
model_C5 = train(Churn ~ ., data=trainData1[-c(15)], method='C5.0')
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
testData2 <- predict(dummies_model, testData)

# Step 3: Transform the features to range between 0 and 1
# testData3 <- predict(preProcess_range_model, testData2)

# View
head(testData2[, 1:10])

#### 6.4. Predict on testData ####
# Predict on testData
predicted <- predict(model_C5, testData2)
head(predicted)

#### 6.5. Confusion Matrix ####
# Compute the confusion matrix
confusionMatrix(reference = testData$Churn, data = predicted, mode='everything', positive='True.')

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
model_C5_2 = train(Churn ~ ., data=trainData1, method='C5.0', tuneLength = 5, metric='ROC', trControl = fitControl)
model_C5_2

# Step 2: Predict on testData and Compute the confusion matrix
predicted2 <- predict(model_C5_2, testData2)
confusionMatrix(reference = testData$Churn, data = predicted2, mode='everything', positive='True.')



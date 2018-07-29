# Load libraries
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)


# Load the data
if(!file.exists("pml-training.csv")){
  url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(url, destfile="./pml-training.csv")
}
if(!file.exists("pml-testing.csv")){
  url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(url,destfile="./pml-testing.csv")
}

train <- read.csv("pml-training.csv", sep=",", header=TRUE, na.strings = c("NA","",'#DIV/0!'))
test  <- read.csv("pml-testing.csv",  sep=",", header=TRUE, na.strings = c("NA","",'#DIV/0!'))
dim(train); dim(test)


# Clean data
train <- train[, colSums(is.na(train)) == 0]
test  <- test [, colSums(is.na(test))  == 0]
dim(train); dim(test)



# Pre-processing
numVars <- which(lapply(train, class) %in% "numeric")
model   <- preProcess(train[, numVars], method = c('knnImpute', 'center', 'scale'))

pTrain        <- predict(model, train[, numVars])
pTrain$classe <- train$classe
pTest         <- predict(model, test[, numVars])


# Create validation set
set.seed(1234)
idxTrain   <- createDataPartition(pTrain$classe, p = 0.75, list = FALSE)
train <- pTrain[idxTrain, ]
valid <- pTrain[-idxTrain, ]
dim(train) ; dim(valid)


# Train the model using random forest
rfFit <- train(classe ~ ., method="rf", data = train, trControl = trainControl(method='cv'), number = 5, allowParallel = TRUE, importance = TRUE )
rfFit


# Plot the importance of each individual variable
varImpPlot(rfFit$finalModel, type = 1, main = "Importance of the Principal Components")


# Cross validation testing
predicted <- predict(rfFit, valid)
conMatrix <- confusionMatrix(valid$classe, predicted)
conMatrix$table

accuracy <- postResample(valid$classe, predicted)
accuracy <- accuracy[[1]]
accuracy

# Apply on test set
prediction <- predict(rfFit, pTest)
prediction








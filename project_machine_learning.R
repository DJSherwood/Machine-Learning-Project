library(randomForest)
library(caret)
library(MASS)

## Read-in Training and Validation Data sets
trn <- read.csv("C:\\Users\\Daniel\\Documents\\R_Files\\Machine_Learning\\pml-training.csv")
vld <- read.csv("C:\\Users\\Daniel\\Documents\\R_Files\\Machine_Learning\\pml-testing.csv")

## Remove prod id, username, etc.
train_data <- trn[,-c(1,2,3,4,5,6,7)]
valid_data <- vld[,-c(1,2,3,4,5,6,7)]

## Find bad columns
bad_seq <- NULL
for (i in 1:(dim(train_data)[2]-1)) {
        
        ## Determine how "filled" a column is
        percent_na <- sum(is.na(train_data[,i]))/(dim(train_data)[1])
        
        ## Boole
        boole <- is.factor(train_data[,i])
        
        ## Standard Deviation
        #sd_seq <- sd(train_data[,i])
        
        ## Add NA columns to bad list
        if (percent_na > 0.30) {
                bad_seq <- c(bad_seq,i)
        }
        
        ## Add factor columns to bad list
        else if (boole == TRUE) {
                bad_seq <- c(bad_seq,i)
        }
        
        ## Add small SD to bad list
        #else if (abs(sd_seq) < 1) {
        #        bad_seq <- c(bad_seq,i)
        #}
}

## Remove "bad" columns
unique_bad_seq <- unique(bad_seq)
real_train_data <- train_data[,-unique_bad_seq]
real_valid_data <- valid_data[,-unique_bad_seq]

## Divide the training data into training and testing
inTrain <- createDataPartition(y=real_train_data$classe, p=0.75, list=FALSE)
training <- real_train_data[inTrain,]
testing <- real_train_data[-inTrain,]

## Control for the train function
fitControl <- trainControl(method="repeatedcv",
                           number = 5, 
                           repeats = 5,
                           classProbs=TRUE,
                           verboseIter=TRUE)

## Produce a model fit
ptm <- proc.time()
set.seed(1)
modelFit <- train(training$classe ~., 
                  data=training, 
                  method="rf",
                  preProcess = c("center","scale"),
                  trControl=fitControl)
proc.time() - ptm

## Test
predictions <- predict(modelFit,newdata=testing[,-46])
predictions2 <- predict(modelFit,newdata=real_valid_data)

## ConfuseMatrix to determine performance
confusionMatrix(predictions,testing$classe)
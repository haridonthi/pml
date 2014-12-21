#Course Project
rm(list=ls())
setwd("C:/Users/Hari/Downloads/Practical Machine Learning")

training=read.csv("pml-training.csv", na.strings=c("#DIV/0!","NA"))

testing=read.csv("pml-testing.csv", na.strings=c("#DIV/0!","NA"))
testing=read.csv("pml-testing.csv")

require(caret)

#Pre-process, step 1
#View near-zero variables
#nearZeroVar(training, saveMetrics = TRUE)
#remove non-zero variables from training dataset
nzv <- nearZeroVar(training)
training1 <- training[, -nzv]
dim(training1)

#Pre-process, step 2 - remove variables where all rows in TEST are NAs - using nzv (hack...)
a <- nearZeroVar(testing, saveMetrics = TRUE)
nullsInTest <- rownames(subset(a,nzv==TRUE))
training2 <- training1[,!(names(training1) %in% nullsInTest)]
dim(training2)
#View(training2)

#Pre-process step 3
# Remove X, user_name, num_window - subjectively
drops <- c("X","user_name","num_window")
training3 <- training2[,!names(training2) %in% drops]

#Train
set.seed(825)

#rf - removed nullsInTest, 2-fold,2-times,
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)
rfFit <- train(classe ~ ., data = training3, method = "rf", trControl = fitControl)
rfFit

#predict

nzv <- nearZeroVar(training)
testing1 <- testing[, -nzv]
testing2 <- testing1[,!(names(testing1) %in% nullsInTest)]
testing3 <- testing2[,!names(testing2) %in% drops]
pred <- predict(rfFit,testing3)
pred

#create files to submit
answers = pred
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

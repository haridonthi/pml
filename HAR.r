#Course Project
setwd("C:/Users/Hari/Downloads/Practical Machine Learning")
training=read.csv("pml-training.csv", na.strings=c("#DIV/0!","NA"))
require(caret)
#Pre-process, step 1
#View non-zero variables
#nearZeroVar(training, saveMetrics = TRUE)
#remove non-zero variables from training dataset
nzv <- nearZeroVar(training)
training1 <- training[, -nzv]
dim(training1)

#Pre-process, step 2
#Use my judgment to remove other variables that "cannot be predictors": X, user_name
#Pre-process, step 3 - remove variables where all rows in TEST are NAs (hack...)
a <- nearZeroVar(testing, saveMetrics = TRUE)
nullsInTest <- rownames(subset(a,nzv==TRUE))
training2 <- training1[,!(names(training1) %in% nullsInTest)]
dim(training2)


#View(training2)

#Train
set.seed(825)

#no pre-processing
rfFit <- train(classe ~.,data=training,method=rf)
rfFit
dim(training)
dim(testing)
pred <- predict(rfFit,testing,na.action=na.omit)

#default rf: accuracy .82
rfFit0 <- train(classe ~ ., data = training2, method = "rf")
rfFit0

#rf, 10-fold cv, 10-times: .86 accuracy (mtry=70)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
rfFit1 <- train(classe ~ ., data = training2, method = "rf", trControl = fitControl)
rfFit1

#rf - removed nullsInTest, 10-foldcv, 10-times,
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
rfFit3 <- train(classe ~ ., data = training2, method = "rf", trControl = fitControl)
rfFit3

#rf, 20-fold cv, 20-times: .87 accuracy
fitControl <- trainControl(method = "repeatedcv", number = 20, repeats = 20)
rfFit2 <- train(classe ~ ., data = training2, method = "rf", trControl = fitControl)
rfFit2

# ada, Currently this procedure can not directly handle > 2 class response
rfFit1 <- train(classe ~ ., data = training2, method = "ada")

#predict
testing=read.csv("pml-testing.csv", na.strings=c("#DIV/0!","NA"))
testing=read.csv("pml-testing.csv")

testing1 <- testing[, -nzv]
testing2 <- testing1[,!(names(testing1) %in% nullsInTest)]
pred <- predict(rfFit2,testing2,na.action=na.omit)
pred

#create files 
answers = rep("A", 20)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

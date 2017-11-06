library(caret)
library(e1071)
library(kernlab)
library(glmnet)
library(nnet)
library(doParallel) # parallel processing
registerDoParallel(cores = 8) # Registrer a parallel backend for train
getDoParWorkers() # check that there are 4 workers

# Function for decision tree using e1071
do.e071.DT <- function(training){
  set.seed(313)
  print("e1071 decision trees")
  tune.control(sampling = c("cross"), cross = 10)
  dec_tree <- tune.rpart(Touchdown ~ . , data = training , minsplit = c(1,2,3),
                         cp = c(0,2^seq(from = -10 , to= 10, by = 2))#, 
                         #minbucket = c(1,3,9),
                         #maxcompete = c(1,3,9), 
                         #maxsurrogate = seq(from = 1 , to= 3, by = 0.5),
                         #usesurrogate = c(0,1,2), 
                         #xval = c(10),
                         #surrogatestyle = c(0,1), 
                         #maxdepth = seq(from = 10 , to= 14, by = 2)
  )
  dec_tree
}

# Function for random forest. 'ntree' must be defined in before running. 
do.RF <- function(training)
{  print("random forest")
  set.seed(313)
  n <- dim(training)[2]
  
  #gridRF <- expand.grid(mtry = seq(from=0,by=as.integer(n/10),to=n)[-1])
  gridRF <- expand.grid(mtry = seq(from=0,by=1,to=n)[-1]) #may need to change this depend on your data size
  ctrl.crossRF <- trainControl(method = "cv",number = 10,classProbs = TRUE,savePredictions = TRUE,allowParallel=TRUE)
  rf.Fit <- train(Touchdown ~ .,data = training,method = "rf",metric = "Accuracy",preProc = c("center", "scale"),
                  ntree = 100, tuneGrid = gridRF,trControl = ctrl.crossRF)
  rf.Fit
}

#radial basis kernel svm 
do.RadialKernelSVM <- function(training)
{
  set.seed(123)
  sigma=sigest(data.matrix(training[,-dim(training)])) # sigest returns 3 values of sigma
  grid <- expand.grid(sigma = sigma , C = 2^seq(from=-1,by = 1, to =5))
  print("Radial Kernel SVM")
  ctrl.cross <- trainControl(method = "cv", number = 5,classProbs = TRUE,savePredictions=TRUE)
  svm.Fit <- train(Touchdown ~ ., data= training,perProc = c("center", "scale"),
                   method = 'svmRadial', #rpart for classif. dec tree
                   metric ='Accuracy',
                   tuneGrid= grid,
                   trControl = ctrl.cross
  )
  svm.Fit
}

#polynomial kernel svm
do.PolyKernelSVM <- function(training)
{
  set.seed(1)
  grid <- expand.grid(scale = 1, degree = c(1,2,3), C = 2^seq(from=-1,by = 1, to =3))
  print("Poly Kernel SVM")
  ctrl.cross <- trainControl(method = "cv", number = 5,classProbs = TRUE,savePredictions=TRUE)
  svm.Fit <- train(Touchdown ~ ., data= training,perProc = c("center", "scale"),
                   method = 'svmPoly', #rpart for classif. dec tree
                   metric ='Accuracy',
                   tuneGrid= grid, 
                   trControl = ctrl.cross
  )
  svm.Fit
}


# Function for K nearest neighbor (KNN)
do.KNN <- function(training){
  print("k nearest neighbor")
  grid <- expand.grid(kmax = seq(from=1,to=5,by = 1),distance = c(1,2),kernel = "optimal")
  set.seed(313)
  ctrl.cross <- trainControl(method = "cv", number = 10,allowParallel=TRUE)
  knn_fit <- train(Touchdown ~ ., data = training,method = "kknn",metric = "Accuracy",perProc = c("center", "scale"),tuneGrid = grid,
                   trControl = ctrl.cross)
  knn_fit
}

#
do.CARET.Predict<-function(tune.model,test_data){
  Pred <- predict(tune.model,test_data)
  cm<- confusionMatrix(Pred,test_dataD$Touchdown)
  cm
}

#
do.e1071.Predict<-function(tune.model,test_data){
  Pred <-  predict(tune.model$best.model,test_data,type="class")
  cm<- confusionMatrix(Pred,test_data$Touchdown)
  cm
}

######################################################

#ridge, make alphaHat = 0,
#lasso, make alphaHat = 1,
#elastic-net make alphaHat between 0 and 1
do.penalized.lr <- function(predictors,response,alphaHat)
{ 
  if(length(unique(response)) < 3)
  { 
    family = "binomial"
  }
  else
  {
    family = "multinomial"
  } 
  lambda = 2^seq(from=-15,to=-1,by=0.1)
  #nlambda is another para
  cv.glmnet.fit <- cv.glmnet(data.matrix(predictors),response, alpha = alphahat, nfolds = 10, type.measure = "class",lambda = lambda, family=family)
  cv.glmnet.fit
  
}

#function to display model summary, also generate some plots.
DisplaySummary <- function(cv.glmnet.fit)
{
  print(cv.glmnet.fit$lambda.min)
  print(cv.glmnet.fit$lambda.1se)
  choice = cv.glmnet.fit$lambda.min
  print(choice)
  dev.new()
  #classification error vs lambda
  plot.cv.glmnet(cv.glmnet.fit,main="error vs lambda")
  # explore cv.glmnet.fit$glmnet.fit
  dev.new()
  #plots how the coefficients vary with lambda
  plot.glmnet(cv.glmnet.fit$glmnet.fit,xvar="lambda",label=TRUE,main="coef vs lambda")
}


#extract NON-ZERO coefficients from optimal model
extractCoefs <- function(cv.glmnet.Fit,family)
{
  tmp <- coef(cv.glmnet.Fit,s="lambda.min")
  #tmp <- coef(cv.glmnet.Fit,s="lambda.1se")
  if(family=="binomial")
  { T <- as.matrix(tmp)
  }
  else
  {T <- as.matrix(tmp[[1]])
  }
  coefs <- data.frame(T[(abs(T[,1]) > 0),])
  features <- row.names(coefs)
  coefs #returns a data frame with row names as feature selected and column1 as coefs in the model
  
}

#returns the confusion matrix of the result
do.Predict<-function(cv.glmnet.fit,predictors,response){
  #get predicted class
  Pred <- predict(cv.glmnet.fit,data.matrix(predictors), type = "class",s=cv.glmnet.fit$lambda.min)
  #get fitted probabilities
  Prob<- predict(cv.glmnet.fit,data.matrix(predictors), type = "response",s=cv.glmnet.fit$lambda.min)
  cm<- confusionMatrix(Pred,response)
  cm
}




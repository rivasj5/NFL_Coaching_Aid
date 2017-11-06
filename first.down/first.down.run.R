setwd("/Users/johnrivas/Desktop/UHD/SCHOOL/Fall_2017/Predictive/Project/GitHub/first.down")
source("first.down.models.R")
training <- read.csv("first.down.train.csv")
testing <- read.csv("first.down.test.csv")
training <- training[,-1]
testing <- testing[,-1]
training$FirstDown <- as.factor(training$FirstDown)
testing$FirstDown <- as.factor(testing$FirstDown)
# training <- training[1:500,] # i used to make sure that everything could run. I commented out once it all worked. Left just in case.
# # There were some errors that i think were due to not using all factor levels in model. I will need more computing power. 

########## Decision Tree
# Train model
dc.fit <- do.e071.DT(training)
# Performance
1-dc.fit$best.performance 
# Best Tuned Model's Parameters
dc.fit$best.parameters
# Plot Accuracy
plot(sort(1-dc.fit$performances[,3]),type = "l", ylab = "Accuracy", xlab = "Worst to Best Parameter Combination", main = "Decision Tree")

# Testing
dc.Fit.Pred <- do.e1071.Predict(dc.fit,testing)
dc.Fit.Pred

########## Random Forest
# Train model
RF.fit <- do.RF(training)
print(RF.fit)

plot(RF.fit$results[,1],RF.fit$results[,2], type = "l", ylab = "Accurary", xlab = "mtry", main = "Random Forest")

# Testing
RF.fit.Pred <- do.CARET.Predict(RF.fit,testing)
RF.fit.Pred
# Accuracy :

########## SVM radial basis kernel
svm.radial.fit <- do.RadialKernelSVM(training)
print(svm.radial.fit)
svm.radial.fit$bestTune

plot(svm.radial.fit$results[,3],type = "l", ylab = "Accuracy", xlab = "Index of Parameter Combination", main = "SVM Radial")

svm.radial.fit$results
# 

# Testing
svm.radial.Pred <- do.CARET.Predict(svm.radial.fit,testing)
svm.radial.Pred
# Accuracy :

########## SVM polynomial kernel
svm.poly.Fit <- do.PolyKernelSVM(training)
print(svm.poly.Fit)
svm.poly.Fit$bestTune

plot(svm.poly.Fit$results[,4],type = "l", ylab = "Accuracy", xlab = "Index of Parameter Combination", main = "SVM Poly")

svm.poly.Fit$results
# Accuracy : 

# Testing
svm.poly.Pred <- do.CARET.Predict(svm.poly.Fit,testing)
svm.poly.Pred
# Accuracy : 

########## KNN
knn.fit <- do.KNN(training)
print(knn.fit)
knn.fit$bestTune

plot(knn.fit$results[,4],type = "l", ylab = "Accuracy", xlab = "Index of Parameter Combination", main = "KNN")

knn.fit$results
# Accuracy :  

# Testing
knn.Pred <- do.CARET.Predict(knn.it,testing)
knn.Pred
# Accuracy :

########## Penalized logistic regression
# Create baseline regression model
fit_logit <- glm(FirstDown ~ ., data = training, family = "binomial")

class <- fit_logit$fitted.values
class[class > 0.5 ] = 1
class[class <= 0.5 ] = 0
accuracyTrain <- confusionMatrix(class, training$Touchdown)
print(accuracyTrain)

# Testing
rankP <- predict(fit_logit, newdata = testing, type = "response")
rankP <- as.matrix(rankP)
colnames(rankP) <- c('class')
rankP[class > 0.5 ] = 1
rankP[class <= 0.5 ] = 0
accuracyTesting <- confusionMatrix( rankP, testing$FirstDown)
print(accuracyTesting)

# Penalized regression
predictors <- training[,-16]
response <- training[,16]
alphahat <- 0.5

#
plr.fit <- do.penalized.lr(predictors,response, alphahat)
extractCoefs(plr.fit,"binomial")
# commented because of many parameters
DisplaySummary(plr.fit)
print(do.Predict(plr.fit,predictors,response))
#print(do.Predict(cv.ridge.fit,predictors_test,response_test) # Commented out because i need to fix later

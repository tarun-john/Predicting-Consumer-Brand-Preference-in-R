complete <- read.csv("CompleteResponses.csv", header = TRUE)
incomplete <- read.csv("SurveyIncomplete.csv", header = TRUE)

#Data Cleaning
complete$elevel <- as.factor(complete$elevel)
complete$car <- as.factor(complete$car)
complete$zipcode <- as.factor(complete$zipcode)
complete$brand <- as.factor(complete$brand)
str(complete) 

incomplete$elevel <- as.factor(incomplete$elevel)
incomplete$car <- as.factor(incomplete$car)
incomplete$zipcode <- as.factor(incomplete$zipcode)
incomplete$brand <- as.factor(incomplete$brand)
str(incomplete)

set.seed(666)

library(randomForest)
library(caret)
library(e1071)

#fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


#rfit1 <- train(complete$brand~., data = complete, method = "rf", 
               trControl = fitcontrol, tuneLength = 1)


#data1 <- rfImpute(complete$brand~., data = complete, iter = 5)

#Random Forest
rf1 <- randomForest(complete$brand~., data = complete, proximity = TRUE)
rf2 <- randomForest(complete$brand~., data = complete, ntree = 400, 
                    mtry = 3, 
                    importance = TRUE, 
                    proximity = TRUE)
#Tuning RF training model
t1 <- tuneRF(complete[,-7], complete[,7], stepFactor = .7, plot = TRUE, 
             ntreeTry = 400, trace = TRUE, improve = 0.05)

#Apply saved model to unseen data
p1 <- predict(rf2, incomplete)

#Plot results
confusionMatrix(p1, incomplete$brand)
plot(rf2, main = "OOB Random Forest")
varImpPlot(rf2, main = "Variable Importance Plot")


MDSplot(rf2, complete$brand)

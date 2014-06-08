# set working directory and load data csv
setwd("~/Desktop/kaggle/titanic/kaggle_titanic")

train <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/train.csv")
test <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/test.csv")

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

prediction <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file="decisiontree20140608.csv", row.names=FALSE)




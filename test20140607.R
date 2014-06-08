
# set working directory and load data csv
setwd("~/Desktop/kaggle/titanic/kaggle_titanic")

train <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/train.csv")
test <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/test.csv")

table(train$Survived)
prop.table(table(train$Survived))

summary(train$Sex)
prop.table(table(train$Sex))
prop.table(table(train$Sex,train$Survived),1)

summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 +Pclass + Sex, data=train, FUN=function(x){sum(x)/length(x)})


#tutorial 1st
#test$Survived <- rep(0, 418)

#tutorial 2nd (female:Survived)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#tutorial 3rd (female Pclass:3 Fare:20+ died)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20 ] <- 0

submit <- data.frame(test$PassengerId, test$Survived)
write.csv(submit, file="women3rdmore20perish.csv", row.names=FALSE)


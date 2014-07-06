# set working directory and load data csv
setwd("~/Desktop/kaggle/titanic/kaggle_titanic")

train <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/train.csv")
test <- read.csv("~/Desktop/kaggle/titanic/kaggle_titanic/test.csv")

#trainとtestを結合
test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
#combi$Name[1]

combi$Title <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
#table(combi$Title)

combi$Title[combi$Title %in% c('Mlle', 'Mme')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
#table(combi$Title)
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <=2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]


library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, 
             data=train, method="class")

#plot(fit)
#text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

prediction <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file="decisiontree20140706.csv", row.names=FALSE)

#lesson5
summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Age)

summary(combi)
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

library(randomForest)
set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+
                      FamilySize+FamilyID2, data=train,importance=TRUE,ntree=2000)

varImpPlot(fit)

prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file="randomforest20140706.csv", row.names=FALSE)

#install.package('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+
                      FamilySize+FamilyID, data=train,controls=cforest_unbiased(ntree=2000,mtry=3))

prediction <- predict(fit, test, OOB=TRUE, type="response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file="cforest20140706.csv", row.names=FALSE)

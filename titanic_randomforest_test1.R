library("randomForest")

#分析対象列のみ、年齢欠損行除去
train_usedonly <- read.csv("/Users/yuji/Desktop/kaggle/titanic/train_pclass_sex_age.csv"
                           , colClasses=c("character", "factor", "factor", "factor", "numeric"))
#分析対象列のみ、年齢欠損行そのまま
test_usedonly <- read.csv("/Users/yuji/Desktop/kaggle/titanic/test_usedonly.csv"
                          , colClasses=c("character", "factor", "factor", "numeric"))
#分析対象列のみ、年齢欠損行除去
test_usedonly_noblankage <- read.csv("/Users/yuji/Desktop/kaggle/titanic/test_usedonly_noblankage.csv"
                          , colClasses=c("character", "factor", "factor", "numeric"))

train.rf <- randomForest(Survived ~ ., data=train_usedonly)
print(train.rf)
plot(train.rf)
varImpPlot(train.rf)
pred.rf <- predict(train.rf, newdata=test_usedonly, type="class")

table(pred.rf)
summary(pred.rf)

write.csv(pred.rf, "/Users/yuji/Desktop/kaggle/titanic/result_test_usedonly.csv")

#分析対象列のみ、年齢欠損行平均値
test_usedonly_agemean <- read.csv("/Users/yuji/Desktop/kaggle/titanic/test_usedonly_agemean.csv"
                                     , colClasses=c("character", "factor", "factor", "numeric"))

#PassengerIdを除去してやり直し
testdfr <- train_usedonly[, c("Survived", "Pclass", "Sex", "Age")]
train.rf2 <- randomForest(Survived ~ ., data=testdfr)
print(train.rf2)
plot(train.rf2)
varImpPlot(train.rf2)

pred.rf2 <- predict(train.rf2, newdata=test_usedonly_agemean, type="class")
table(pred.rf2)
summary(pred.rf2)

write.csv(pred.rf2, "/Users/yuji/Desktop/kaggle/titanic/result_test_usedonly_agemean.csv")



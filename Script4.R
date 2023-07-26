분류모형 (classification)

 - 로지스틱회귀모형 : 속할 확률, 선형식을 통해 계산 * 
 - k-최근접법 : 모형이 아님 * 
  (새로운 자료와 기존 자료 비교 : 가장 가까운 k을 선택)

library(DMwR2)
knn.m <- kNN(Type~.-Dilution, train.wine, test.wine, k=7)
maps::map('world', 'south korea')

 - 나이브베이브 (text 분류) *
 - 의사결정나무 * 
 - Support Vector Machine *
 - 랜덤포레스트 *

head(wine)
library(nnet)

train.wine (70%)
test.wine  (30%)

# 학습 : train data로 모형
logic.m <- multinom(Type~.-Dilution, train.wine)
predict(logic.m, test.wine) # 예측값
test.wine$Type # 관측값

table(test.wine$Type, predict(logic.m, test.wine))
library(caret)
confusionMatrix(predict(logic.m, test.wine), test.wine$Type)

#####################################
# 의사결정나무
library(rpart)
dt.rpart <- rpart(Type~.-Dilution, train.wine)
dt.rpart %>% plot
text(dt.rpart)

table(test.wine$Type, predict(dt.rpart, test.wine, type="class"))
confusionMatrix(predict(dt.rpart, test.wine, type="class"), test.wine$Type)

library(party)
rt.ctree <- ctree(Type~.-Dilution, train.wine)
rt.ctree %>% plot
table(test.wine$Type, predict(rt.ctree, test.wine))

library(tree)
dt.tree <- tree(Type~.-Dilution, train.wine)
plot(dt.tree)
text(dt.tree)
table(test.wine$Type, predict(dt.tree, test.wine, type="class"))

#########################################
# 나이브 베이즈
library(e1071)
nb.model <- naiveBayes(Type ~ .-Dilution, train.wine)
predict(nb.model, test.wine)

svm.model <- svm(Type~.-Dilution, train.wine)
predict(svm.model, test.wine)

Random Forest 숲, 여러개의 의사나무 (앙상블 모형)
 * 90% random 선택
 * 변수도 랜덤으로 선택하는 모형 

library(randomForest)
rf.model <- randomForest(Type~.-Dilution, train.wine, ntree=300)
predict(rf.model, test.wine)

confusionMatrix(predict(rf.model, test.wine), test.wine$Type)

# mtry : hyperparameter
rf.model1 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=1)
rf.model2 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=2)
rf.model3 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=3)
rf.model4 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=4)
rf.model5 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=5)
rf.model6 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=6)
rf.model7 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=7)
rf.model8 <- randomForest(Type~.-Dilution, train.wine, ntree=300, mtry=8)

table(predict(rf.model8, test.wine), test.wine$Type)

# grid, random, hamonic 

plot(dist~speed, cars)
m1 <- lm(dist~speed, cars)
m2 <- svm(dist~speed, cars)
m3 <- randomForest(dist~speed, cars)
newdata = data.frame(speed=seq(5,25,0.5))

y1 <- predict(m1, newdata)
y2 <- predict(m2, newdata)
y3 <- predict(m3, newdata)

lines(newdata$speed, y1, col="blue", lwd=2)
lines(newdata$speed, y2, col="red", lwd=2)
lines(newdata$speed, y3, col="gold", lwd=2)


head(iris)
iris$Species %>% table

set.seed(1537)
sel <- sample(1:150, 120)

train.iris <- iris[sel,]
test.iris <- iris[-sel,]

# 분류모형 세우기

# 로지스틱 
logistic.m <- multinom(Species~., train.iris)
table(test.iris$Species, predict(logistic.m, test.iris))

# 의사결정나무
dt.m1 <- rpart(Species~., train.iris)
table(test.iris$Species, predict(dt.m1, test.iris, type="class"))

dt.m2 <- ctree(Species~., train.iris)
table(test.iris$Species, predict(dt.m2, test.iris))

# support vector machine
svm.m <- svm(Species~., train.iris)
table(test.iris$Species, predict(svm.m, test.iris))

# random forest
rf.m <- randomForest(Species~., train.iris)
table(test.iris$Species, predict(rf.m, test.iris))

library(caret)

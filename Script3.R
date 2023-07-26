Y = f(X) : 회귀분석
Y : 범주형 (분류모형)

fisher의 분류함수로 분류모형 : 다변량정규분포

로지스틱 회귀모형
의사결정나무
랜덤포레스트
Support Vector Machine
K-근접법 

특정 집단에 속할 확률 : p (선형식)

library(nnet)
# 훈련
logit.m <- multinom(factor(cyl)~mpg+disp+hp+drat+wt+qsec+factor(gear), mtcars)
logit.m %>% summary

round(predict(logit.m, mtcars, type="probs"),3)

0<= exp(선형식) <Inf

exp/(1+exp) = p

(1+exp) / exp = 1/p

1/exp(x) + 1 = 1/p
1/exp(x) = 1/p -1
1/exp(x) = p-1/p
exp(x) = p/(p-1)
x = log(1/(p-1)) #로짓변환

sel <- sample(1:nrow(wine), 130)

train.wine <- wine[sel,] # train data
test.wine <- wine[-sel,] # test data

logit.m <- multinom(Type~., data=train.wine)
predict(logit.m, test.wine)
table(test.wine$Type, predict(logit.m, test.wine))


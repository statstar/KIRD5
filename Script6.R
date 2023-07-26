# 주성분분석 : 독립변수의 수를 축소 
pcadata <- princomp(iris[,1:4], cor=T)
pcadata %>% summary

# summary
plot(pcadata, type='l')

# 주성분 이용할 건지 결정
## 1. Standard deviation >=1
## 2. 누적확률이 60~80%
## 3. 스크리 도표의 팔꿈치 지점 (경사가 완만해 지는 주성분 수)

pcadata$loadings[,1:2]
pcadata$scores[,1:2] %>% head

biplot(pcadata)
head(iris[,1:4])

homework <- read.csv("C:/Users/stats/Downloads/KIRD5-main (1)/KIRD5-main/open_closed.csv")

pcadata <- princomp(homework, cor=T)
pcadata %>% summary
plot(pcadata, type="l")
pcadata$loadings[,1:2]

biplot(pcadata)
pcadata$scores[,1:2] 

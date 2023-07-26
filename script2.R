# 군집분석
 - 계층적 군집분석 hclust
1    2          5    7         10    16  
|____|          |    |         |      |
                |____|         |      |
                   |___________|      |
                          |___________|
  
Dendrogram
  * 거리를 계산하기 위한 변수 선정
  * 거리계산 (유클리디언 거리)
  * 객체간 어떤 거리를 이용할건지 (최단, 최장, 평균)
  * 군집의 수

data(nutrient,package="flexclust")
summary(nutrient)

head(nutrient,4)
nutrient_s <- scale(nutrient) # 표준화
nutrient_d <- dist(nutrient_s) # 유클리디언 거리
result1 <- hclust(nutrient_d, method="single")
result2 <- hclust(nutrient_d, method="complete")
result3 <- hclust(nutrient_d, method="average")

plot(result1, hang=-1, cex=0.7)
plot(result2, hang=-1, cex=0.7)
plot(result3, hang=-1, cex=0.7)

clusters<-cutree(result1,k=3)
table(clusters)
rect.hclust(result1,k=3)

library(NbClust)
nc=NbClust(nutrient_s, distance="euclidean",
           min.nc=2, max.nc=10,
           method="average")

 - 비계층적 군집분석 : k-means 군집분석, mclust, DBSCAN

kmeans(nutrient, 3) # 거리를 이용해서 군집분석
km <- kmeans(nutrient_s, 3) # 거리를 이용해서 군집분석
km$cluster
km$centers
  
nutrient %>% 
  filter(km$cluster==3)

# k-means 한계점 
  거리를 이용 (표준화)
  군집의 수 NbClust 
  변수별 가중치 동일하게 설정
  
# Model based clustering (Gaussian Mixture Model)
  Density (표준화 필요성이 없음)
  BIC 기존으로 최적 군집 수
  변수 별 가중치가 다르게 설정
  
library(mclust)
mc = Mclust(nutrient)
summary(mc)
mc$classification
plot(mc)

library(factoextra)
# 데이터 많으면 계산 시간이 오래 걸림

data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")

library(fpc)
library(dbscan)
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)
db$cluster %>% table

# 군집분석 수행해보기
data(wine,package="rattle")

wine2 <- wine[,-1]
wine$Type %>% table

# wine2 군집분석 (비지도학습)
wine2_s <- scale(wine2) # 표준화
NbClust(wine2_s, method="kmeans") # 최적 군집 수 3 확인

km.cluster <- kmeans(wine2_s, 3)
table(wine$Type, km.cluster$cluster) # 6개 miss matching
km.cluster$centers

mc.cluster <- Mclust(wine2)
table(wine$Type, mc.cluster$classification) # 2개 miss matching
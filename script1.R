library(dplyr)

데이터 유형 
  - 범주형 (문자형)
    table(mtcars$cyl)
    table(mtcars$cyl) %>% barplot
    table(mtcars$cyl, mtcars$vs)
    table(mtcars$cyl, mtcars$vs) %>% 
      barplot(beside=T, legend=rownames(.))
    table(mtcars$cyl, mtcars$vs) %>% chisq.test
  
  - 연속형 (수치형)
    summary(mtcars$wt)
    hist(mtcars$wt)
    boxplot(mtcars$wt) # 이상치 
    vioplot(mtcars$wt) # library(vioplot)

# 이변량 분석
    범주형 * 연속형 : Mean difference (평균 비교)
    집단이 2개 : t.test

    boxplot(wt~vs, mtcars)
    mtcars %>% 
      group_by(vs) %>% 
      summarise(n=n(), mean=mean(wt), sd=sd(wt))
    var.test(wt~vs, mtcars)     # 등분산성 검정: p>0.05 (등분산성 만족)
    t.test(wt~vs, mtcars, var.equal=T) #유의확률 < 0.05 (평균차이가 있다)
    
    집단이 2개 이상 : one way ANOVA
    boxplot(wt~gear, mtcars)
    mtcars %>% 
      group_by(gear) %>% 
      summarise(n=n(), mean=mean(wt), sd=sd(wt))
    bartlett.test(wt~gear, mtcars)
    aov(wt~factor(gear), mtcars) -> result1 # 등분산성 만족
    oneway.test(wt~gear, mtcars) -> result2 # 등분산성 미만족
    
    result1 %>% summary # 분산분석표 확인
    TukeyHSD(result1)
    
    # shapiro.test 정규성 검정
    mtcars %>% filter(gear==3) -> mtcars3
    mtcars %>% filter(gear==4) -> mtcars4
    mtcars %>% filter(gear==5) -> mtcars5
    
    mtcars3$wt %>% shapiro.test()
    mtcars4$wt %>% shapiro.test()    
    mtcars5$wt %>% shapiro.test()    
    
    kruskal.test(wt~gear, mtcars) # 비모수적 접근
    
  # 연속형 * 연속형 
    plot(mpg~wt, mtcars) # scatter plot (산점도)
    cor.test(mtcars$mpg, mtcars$wt)

    out <- lm(mpg^0.2~wt+I(wt^2), mtcars)    # 회귀분석
    summary(out)  # 회귀식, 결정계수 
    predict(out, newdata=data.frame(wt=c(4.2,4.5,4.6)),
            interval="confidence")
    predict(out, newdata=data.frame(wt=c(4.2,4.5,4.6)),
            interval="prediction")

    정규성, 독립성, 등분산성
    library(car)
    out$residuals %>% shapiro.test    
    durbinWatsonTest(out)    
    ncvTest(out)    

    boxCox(out)    
    
    mpg~wt # 독립성 만족 X
    mpg^0.2 = wt + wt^2 # 정규성, 독립성, 등분산성
    
    # 좋은 데이터를 이용
    influencePlot(out) -> sel
    
    mtcars %>% rownames %in% rownames(sel) %>% which
    
    out1 <- lm(mpg^0.2~wt+I(wt^2), mtcars[c(-8,-16,-17), ])    # 회귀분석

    # 독립변수가 여러개 : 다중회귀모형
    - 다중공선성 : vif()/ lasso rigde 변수선택
    - 모형선택 : 변수 선택 (전진선택법, 후진제거법, 단계적변수선택법)
      모형선택기준 : 수정된결정계수, bic, Cp
    - 모형 검증 방법 
      Train(70) / Test(30)
    - 변수중요도 
      scale (표준화 후 lm)
      
      
Import -> Tidy -> Model -> Communicate

head()  /filter
dim()   /select
         mutate, arrange, group_by, summarise
         
  
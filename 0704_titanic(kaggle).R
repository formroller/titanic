
getwd()
setwd('../titanic')
## 2.1 packages
```{r message = FALsE, warning = FALSE}

install.packages(c('readr','descr', 'VIM','ggplot2','RColorBrewer', 'scales', 'tidyversr','dplyr','purrr','tidyr','randomForest','caret','ROCR'))

# data input, assesment
library(readr)  # data input with readr :: read_csv()
library(descr)  # descr::CrossTable() - 범주별 빈도수, 비율을 수치로 확인

# visualization
library(VIM)            # missing vlaues assesment used by VIM::aggr()
library(ggplot2)        # Used in almost visualization
library(RColorBrewer)   # plot color
library(scales)         # plot setting - x/y축 설정

# Feature engineering, Data Pre-Processing
#library(tidyversr)
library(dplyr)   # feature Engineering & Data Pre-Processing
library(purrr)   # Check missing values
library(tidyr)   # tidyr::gater() //https://gomguard.tistory.com/229

# Model generation
library(randomForest)
```



# 한 화면에 여러개의 plot 출력
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL){
    library(grid)
    
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    if(is.null(layout)) {
        layout <- matrix(seq(1, cols * celling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1){
        print(plots[[1]])
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        for (i in 1:numPlots){
            matchidx <- as.data.frame(which(layout == i , arr.ind =TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}



##2.2 raw data impory : 원본데이터 불러오기
train <- readr::read_csv('../titanic/train.csv')
test <- readr::read_csv('../titanic/test.csv')

 # readr::read_csv() : read.csv()보다 메모리에 더 빨리 올릴 수 있다.
 #                   chr / factor 구분 못하고 chr속성으로 저장 

full<- dplyr::bind_rows(train,test)

 # dplyr::bind_rows : rbind()는 차원(dimension)이 맞지않아 병합불가 
 #                  : test의 Survived를 NA로 처리하며 하나로 병합

##2.3 변수 의미
| 변수명        | 해석(의미)                       | Type      | 
    |:-------------:|:---------------------------------|:----------|
    |**PassengerID**|승객을 구별하는 고유 ID number    |Int        |
    |**Survived**   |승객의 생존 여부를 나타내며 생존은 1, 사망은 0 입니다.|Factor|
    |**Pclass**     |선실의 등급으로서 1등급(1)부터 3등급(3)까지 3개 범주입니다.|Ord.Factor|
    |**Name**       |승객의 이름                       |Factor|
    |**Sex**        |승객의 성별                       |Factor|
    |**Age**        |승객의 나이                       |Numeric|
    |**SibSp**      |각 승객과 동반하는 형제 또는 배우자의 수를 설명하는 변수이며 0부터 8까지 존재합니다.                            |Integer|
    |**Parch**      |각 승객과 동반하는 부모님 또는 자녀의 수를 설명하는 변수이며 0부터 9까지 존재합니다.                            |Integer|
    |**Ticket**     |승객이 탑승한 티켓에 대한 문자열 변수|Factor|
    |**Fare**       |승객이 지금까지 여행하면서 지불한 금액에 대한 변수|Numeric|
    |**Cabin**      |각 승객의 선실을 구분하는 변수이며 범주와 결측치가 너무 많습니다.                                          |Factor|
    |**Embarked**   |승선항, 출항지를 나타내며 C, Q, S 3개 범주이다.|Factor|


##2.4 Change the variable type : 변수 속성 변환

# EDA와 feature engineering에 앞서 변수 속성 변환
# data들을 input하면서 `full`에는 `factor` 속성의 변수들이 없기도 하고 
# `Pclass`의 1, 2, 3은 1등급, 2등급, 3등급을 나타내는 `factor`이기 때문입니다.

```{r}
    full <- full %>%
        dplyr::mutate(Survived = factor(Survived),
                      Pclass   = factor(Pclass, ordered = T),
                      Name     = factor(Name),
                      Sex      = factor(Sex),
                      Ticket   = factor(Ticket),
                      Cabin    = factor(Cabin),
                      Embarked = factor(Embarked))
```

##3. EDA
# dat구성과 결측치 혹은 이상치 존재하는지 등을 원시 데이터에서 탐색하고 이해하는 과정

##3.1 수치값을 활용한 data 확인
head(), summary()등 함수들의 결괏값(Output)을 통해 data 확인

##3.1.1 head()
```{r}
head(full, 10)
```
 # Age - NA 존재

##3.1.2 str()
str(full)

##3.1.3 summary()
summary(full)

1. Survived : (target), 418(missing values)는 test data
2. Pclass   : 1,2,3등급으로 범주형이며, 3등급이 가장 많다
3. Name     : 이름이 비슷한 승객 존재 -> 가족과 같이 탑승한 승객 있을것으로 유추
4. Sex      : 남성이 여성보다 2배 많다.
5. Age      : 0.17 - 80세까지 존재/ 0.17은 오기입 or 이상치인지 확인 필요 / 263개의 결측치
6. Sibsp    : 0-8, 3분위수 : 1, 부부 또는 형제와 함께 탑승했음을 유추 가능
7. Parch    : 0-9, 3분위수 : 0, 부모 또는 자녀와 함께 탑승한 승객 거의 없다.
8. Ticket   : 3.1.2str() - 완전 동일, 일부 동일, 완전 다름 => Ticket Size로 파생변수 생성
9. Fare     : 0 - 512/ 1개의 결측치 / 3분위 : 31.275 / 이상값 : 512
10.Cabin    : 가장 많은 결측치(1014)/ 배의 구간 탈락 고려
11.Embarked : 3개의 범주로 구성 / S가 가장 많음 / 2개의 결측치

##3.2 Missing Values
#1) vim
#2) tidyverse - 변수별 결측치 비율 도출
결측치가 있는 변수 확인하며 얼마나 존재하는지 확인

##3.2.1 VIM Pacakages
dev.new()
VIM::aggr(full, prop = FALSE, combined = T, numbers = T, sortVars = T, sortCombs = T)

##3.2.2 tidyverse pakages

full %>%
    dplyr::summarize_all(funs(sum(is.na(.))/n()))

#1) feature의 결측치 비율 계산 ->  Data Frame 속성(1 * 12)
missing_values <- full %>% 
    dplyr::summarize_all(funs(sum(is.na(.))/n()))

#2) 위 missing_values -> 12*2 data frame로  생성    
missing_values <-tidyr::gather(missing_values, key = 'feature', value = 'missing_pct')

#3) missing_values 이용한 시각화
missing_values %>% 
    #Aesthetic settinfg : Missing_pct 내림차순으로 정렬
    ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
    #Bar Plot
    geom_bar(stat = 'identity', fill = 'red') +
    #Title generation
    ggtitle('Rate of missing values in each features')+
    #Title detail setting
    theme(plot.title = element_text(face = 'bold', # 글씨체
                                    hjust = 0.5,   # Horizon(가로비율)
                                    size = 15, color = 'darkblue')) +
    #x, y axis label setting 
    labs(x = 'Feature names', y = 'rate') +
    #plot의 x,y축 변환
    coord_flip()
 # => feature의 결측치 비율 확인 가능
 # => purrr패키지를 통한 결측치 비율 계산 후, 결측치 존재하는 변수만 추출 후 시각화
purrr::map_dbl(full, function(x){round((sum(is.na(x))/length(x)) * 100,1) })

# 1) 변수별 결측치 비율 계산
miss_pct <- purrr::map_dbl(full, function(x){round((sum(is.na(x))/length(x)) * 100, 1) })

# 2) 결측치 비율이 0%보다 큰 변수만 선택
miss_pct <- miss_pct[miss_pct >0]

# 3)Data Frame 생성
data.frame(miss = miss_pct, var = names(miss_pct), row.names = NULL) %>%
    # Aesthetic setting : miss 내림차순으로 정렬
    ggplot(aes(x = reorder(var, miss), y = miss)) +
    # Bar Plot
    geom_bar(stat = 'identity', fill = 'red') +
    # Plot title setting
    ggtitle('Rate of Missing Values') + 
    # Title detail setting
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5,
                                    size = 15, color = 'darkblue'))+
    #x,y axis label setting
    labs(x = 'Feature names', y = 'Rate of missing values') +
    #plot의 x,y축 변환
    coord_flip()

 # => 12개의 변수중 4개 변수에서 결측치 관찰('Survived'는 'test' data 이므로 제외) 'Cabin', 'Age', 'Embarked', 'Fare'

## 시각화 통해 feature를 분석, 탐색 과정

#3.3 Age

dev.new()
age.p1 <- full %>%
    ggplot(aes(Age)) + 
    # 히스토그램 그리기, 설정
    geom_histogram(breaks = seq(0, 80, by = 1),  # 간격 설정
                   col = 'red',                  # 막대 경계선 색깔
                   fill = 'green',               # 막대 내부 색깔
                   alpha = .5) +                 # 막대 투명도 = 50% 
    #Plor title
    ggtitle ('All Titanic passengers age histogram') +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5,
                                    size = 15, color = 'darkblue'))
age.p2 <- full %>%
    # test data set 의 Sruvived == NA 인 값 제외 
    filter(!is.na(Survived)) %>%
    ggplot(aes(Age, fill = Survived)) +
    geom_density(alpha = .5)+
    ggtitle('Titanic passengers age density plot') +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5,
                                    size = 15, color = 'darkblue'))

# multiplot.layout 형식 지정
multi.layout = matrix(c(1,1,2,2), 2,2, byrow = T)

# 위에서 생성한 2개의 그래프 한 화면에 출력
multiplot(age.p1, age.p2, layout = multi.layout)


##3.4 Pclass
# Pclass에 해당하는 탑승객의 빈도수 시각화
# - dplyr 패키지 활용, Pclass별로 그룹화 한 뒤 범주별 빈도수 나타내는 Data Frame 생성 , ggplot으로 시각화

full %>% 
    #dplyr::group_by(), summarize() 이용해 Pclass 빈도수 구하기
    group_by(Pclass) %>%
    summarize(N = n()) %>%
    # Aesthetic setting
    ggplot(aes(Pclass, N))+
    geom_col() +
    # Pclass 빈도수 plot에 출력
    geom_text(aes(label = N),      # Plot의 y에 해당하는 N(빈도수)를 매핑
              size = 5,            # 글씨 크기
              vjust = 1.2,         # vertical(가로) 위치 설정
              color = '#ffffff') + # 글씨 색 : 흰색
    # Plot title
    ggtitle('Number of each Pclass\'s passengers') + 
    # Title setting
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15)) +
    #x, y axis name change
    labs(x ='Pclass', y = 'Count')

 # => 3등급 객실에 탑승한 승객이 가장 많음
 # 객실 등급별 생존율은 추후 다룸

##3.5 Fare
# Fare에 대한 시각화(히스토그램, 상자그림)

#histogram
Fare.p1 <- full %>%
    ggplot(aes(Fare))+
    geom_histogram(col = 'yellow',
                   fill = 'blue',
                   alpha = .5)+
    ggtitle('Histogram of Passengers Fare')+
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15))

 #boxplot
Fare.p2 <- full %>%
    filter(!is.na(Survived)) %>%
    ggplot(aes(Survived, Fare)) +
    # 관측치를 회색점으로, 중복되는 부분은 퍼지게 출력
    geom_jitter(col = 'grey') +
    # 상자그림 투병도 : 50%
    geom_boxplot(alpha = .5) +
    ggtitle('Boxplot of passengers Fare')+
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15))

#multiplot layout 형식 지정
multi.layout = matrix(c(1,1,2,2),2,2)
# 위에서 생성한 그래프 한 화면에 출력
dev.new()
multiplot(Fare.p1, Fare.p2, layout = multi.layout)

# => 생존자들이 사망한 승객들보다 Fare 높지만 큰 차이는 없음

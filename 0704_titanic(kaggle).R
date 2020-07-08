# refer : https://www.kaggle.com/redhorse93/r-titanic-data/code
getwd()
setwd('../titanic')
## 2.1 packages


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


    full <- full %>%
        dplyr::mutate(Survived = factor(Survived),
                      Pclass   = factor(Pclass, ordered = T),
                      Name     = factor(Name),
                      Sex      = factor(Sex),
                      Ticket   = factor(Ticket),
                      Cabin    = factor(Cabin),
                      Embarked = factor(Embarked))


##3. EDA
# dat구성과 결측치 혹은 이상치 존재하는지 등을 원시 데이터에서 탐색하고 이해하는 과정

##3.1 수치값을 활용한 data 확인
# head(), summary()등 함수들의 결괏값(Output)을 통해 data 확인

##3.1.1 head()

head(full, 10)

 # Age - NA 존재

##3.1.2 str()
str(full)

##3.1.3 summary()
summary(full)

# 1. Survived : (target), 418(missing values)는 test data
# 2. Pclass   : 1,2,3등급으로 범주형이며, 3등급이 가장 많다
# 3. Name     : 이름이 비슷한 승객 존재 -> 가족과 같이 탑승한 승객 있을것으로 유추
# 4. Sex      : 남성이 여성보다 2배 많다.
# 5. Age      : 0.17 - 80세까지 존재/ 0.17은 오기입 or 이상치인지 확인 필요 / 263개의 결측치
# 6. Sibsp    : 0-8, 3분위수 : 1, 부부 또는 형제와 함께 탑승했음을 유추 가능
# 7. Parch    : 0-9, 3분위수 : 0, 부모 또는 자녀와 함께 탑승한 승객 거의 없다.
# 8. Ticket   : 3.1.2str() - 완전 동일, 일부 동일, 완전 다름 => Ticket Size로 파생변수 생성
# 9. Fare     : 0 - 512/ 1개의 결측치 / 3분위 : 31.275 / 이상값 : 512
# 10.Cabin    : 가장 많은 결측치(1014)/ 배의 구간 탈락 고려
# 11.Embarked : 3개의 범주로 구성 / S가 가장 많음 / 2개의 결측치

##3.2 Missing Values
#1) vim
#2) tidyverse - 변수별 결측치 비율 도출
# - 결측치가 있는 변수 확인하며 얼마나 존재하는지 확인

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
    geom_bar(stat = 'identity', fill = 'red') +  ## geom_bar 막대그래프, https://rfriend.tistory.com/69 변수 갯수 및 데이터 형태에 따른 그래프
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

## 3.6 Sex
# 남/여 간 생존율 차이 확인

sex.p1 <- full %>%
    dplyr::group_by(Sex) %>%
    summarize(N = n()) %>%
    ggplot(aes(Sex,N)) +
    geom_col()+
    geom_text(aes(label = N), size = 5, vjust = 1.2, color = '#FFFFFF') + 
    ggtitle('Bar Plot of Sex') + 
    labs(x = 'Sex', y = 'Count')

sex.p2 <- full[1:891,] %>%
    ggplot(aes(Sex, fill = Survived)) +
    geom_bar(postion = 'fill') + 
    scale_fill_brewer(palette = 'Set1')+
    scale_y_continuous(labels = percent) + 
    ggtitle('Survival Rate by Sex') + 
    labs(x = 'Sex', y = 'Rate')


multi.layout = matrix(rep(c(1,2), times = 2), 2, 2, byrow = T)

dev.new()
multiplot(sex.p1, sex.p2, layout = multi.layout)

mosaicplot(Survived ~ Sex, 
           data = full[1:891, ], col = TRUE,
           main = 'Survival rate by pessengers gender')

#=> 탑승객은 남성이 높으나 생존율은 여성 탑승객이 높음.

## Featrue Engineering & Data Pre-procseeing
#3-EDA 바탕으로 결측치 대치 및 파생변수 생성

# 4.1 Age -> Age.Group

full <- full %>%
    # 결측치 제외한 값들의 평균으로 채움.
    mutate(Age = ifelse(is.na(Age), mean(full$Age, na.rm = TRUE), Age),
           # Age값에 따라 범주형 파생변수 Age.Group 생성
           Age.Group = case_when(Age < 13           ~ 'Age.0012',
                                 Age >=13 & Age <18 ~ 'Age.1317',
                                 Age >=18 & Age <60 ~ 'Age.1859',
                                 Age >=60           ~ 'Age.60inf'),
           # Chr 속성을 Factor로 변환
           Age.Group = factor(Age.Group))

#  4.2 SibSp & Parch -> FamilySized

full <- full %>%
    #SibSp, Parch와 1(본인)을 더해 FamilySize라는 파생변수 먼저 생성
    mutate(FamilySize = .$SibSp + .$Parch +1,
           # FamilySize 값에 따라 범주형 파생 변수 FamilySize 생성
           FamilySized= dplyr::case_when(FamilySize == 1 ~ 'Single',
                                         FamilySize >= 2 & FamilySize < 5 ~ 'Small',
                                         FamilySize >= 5 ~ 'Big'),
           # Chr 속성인 FamilySize를 Factor로 변환
           # 집단 규모 크기에 따라 levels를 새로 지정
           FamilySize = factor(FamilySized, levels = c('Single','Small','Big')))

# => 'SibSp', 'Parch'를 이용해 FamilySize생성
 # => 두개의 변수를 하나로 줄일 경우 모델이 더욱 단순해지는 장점이 있다.(ex - BMI지수(키,체중))

# 4.4 Name & Sex -> title
#3.6-Sex) 여성의 생존율이 남성보다 높다.
# 따라서 'Name'에서 '성별과 관련된 이름만을 추출해 범주화 시킬경우 유의미 할 것이다'라는 가정
# 먼저 'full' data에서 'Name'이라는 열벡터만 추출해 'title'로 지정

# 1) Name만 추출해 title 벡터에 저장
title <- full$Name
# 2) 정규식, gsub() 이용해 성별과 관련성 높은 이름만 추출해 title 벡터로 저장
title <- gsub('^.*, (.*?)\\..*$','\\1',title)
#3) 위 title 벡터를 full에 다시 저장하되 title 파생변수로 저장
full$title <- title
# 4) 고유값 확인
unique(full$title)
# => 18개의 범주가 있음
# 이를 그대로 사용할 경우 모델의(특히 Tree Based Model)복잡도 상당히 높아짐(변수를 줄여야한다).
# 'descr' 패키지를 이용해 각 범주별 빈도수와 비율 확인

descr::CrossTable(full$title)
=> 18 -> 5개로 범주 축소

# 5) 5개 범주로 단순화 
full <- full %>%
    # in 대신 == 사용시 Recyling Rule(재사용규칙)으로 인해 원하는 값이 나오지 않는다.
    mutate(title = ifelse(title %in% c('Mlle','Ms','Mr','Lady','Dona'), 'Miss',title),
           title = ifelse(title == 'Mme','Mrs', title),
           title = ifelse(title %in% c('Capt','Col','major','Dr','Rev','Don',
                                       'Sir','the Countess','Jonkheer'), 'Officer',title),
           title = factor(title))
# 6) 파생변수 생성 후 범주별 빈도수, 비율 확인
descr::CrossTable(full$title)

# 4.5 Ticket -> Ticket.size
# trian/test 포괄해 승객은 1309명, 일부 중복된 티켈 번호 존재

length(unique(full$Ticket)) # 고유한 범주의 갯수 파악하기 위해 length사용
head(summary(full$Ticket), 10) 

# (확인)
# 결측치 없는 변수이나 고유한 티켓이 929개?
# CA. 2343으로 완전히 같은 인원 11명?

full %>%
    # 티켓이 일치하는 11명의 승객들만 필터링
    filter(Ticket == 'CA. 2343') %>%
    # 모든 변수 확인한 필요 없으므로 아래 변수만 확인
    select(Pclass, Name, Age, FamilySized)

#=> 위 동일한 티켓번호 11명은 가족, 형제 관계
# 그러나, 일부만 일치하는 승객도 존재.
# 이런 티켓의 고유한 넘버(글자수) 갯수를 나타내는 'ticket.unique' 파생 변수 만들고
# 'ticket.unique'를 바탕으로 3개 범주를 갖는 'ticket.size'파생변수를 만들어 보자

#1) 우선 ticket.unique가 모두 0이라고 저장함.
ticket.unique <- rep(0, nrow(full))
#2) Ticket Feature에서 고유한 것들만 추출해 ticket 벡터에 저장
tickets <- unique(full$Ticket)
# 3) 반복문 중첩해 티켓이 같은 승객들만 추출 후, 각 티켓들의 길이(문자 갯수)를 추출해 저장한다.
for (i in 1:length(tickets)){
    current.ticket <- tickets[i]
    party.indexes <- which(full$Ticket == current.ticket)
    #for loop 중첩
    for (k in 1:length(party.indexes)){
        ticket.unique[party.indexes[k]] <- length(party.indexes)
    }
}

# 4) 위에서 계산한 ticket.unique을 파생변수로 저장
full$ticket <- ticket.unique

# 5) ticket.unique에 따라 세가지 범주로 나눠 ticekt.size 변수 생성
full <- full %>%
    mutate(ticket.size = case_when(ticket.unique == 1 ~ 'Single',
                                   ticket.unique < 5 & ticket.unique >= 2 ~ 'Small',
                                   ticket.unique >= 5 ~ 'Big'),
           ticket.size = factor(ticket.size,
                                levels = c('Single','Small','Big')))

# 4.6 Embarked
# 결측치가 2개 있는 feature로, 'Embarked' 범주중 최빈값인 'S'로 치환
full$Embarked <- replace_na(full$Embarked,'S')

# 4.7 Fare 
# 결측치 1개, 위에서 본 히스토그램을 바탕으로 결측치 0으로 치환
full$Fare <- replace_na(full$Fare,0)

# ```데이터 전처리 완료!
#     이후 과정은 지금까지 만든 파생 변수들을 탐색하며 모델 생성에 사용할 변수들을 선택하는 과정(=Feature Selection)```

## 5. Relationship to tsrget feature 'Survived' & Feature Selection
# 본격적인 시각화에 아서, 변수들이 생존율과 얼마나 연관성이 높은지 보는것이 목적.
# 따라서 생존 여부를 알 수 있는 train data set만 사용한다.

#5.0 Data Set Split
# - 아래 코드를 이용해 전처리가 끝난 'full' data를 'train'/'test'로 분할

# Feature Selection 전이므로 모든 변수 선택한다
train <- full[1:891,]
test <- full[892:1309,]

# 5.1)Pclass
dev.new()
train %>%
    ggplot(aes(Pclass, fill = Survived)) +
    geom_bar(position = 'fill') +
    #plot 테마 설정 : 조금 더 선명한 색깔로 변환
    scale_fill_brewer(palette = 'Set1') +
    # Y axis setting
    scale_y_continuous(labels = percent) + 
    # x, y 축 이름과 plot의 main title, sub title 설정
    labs(x = 'Pclass', y = 'Rate',
         title = 'Bar Plot', subtitle = 'How many people survived in each Pclass?')

# 5.2) Sex
# 3.6 - Sex와 동일
mosaicplot(Survived ~ Sex,
           data = train, col = TRUE,
           main = 'Survived rate by passengers gender')

# 5.3) Embarked
train %>% 
    ggplot(aes(Embarked, fill = Survived)) +
    geom_bar(position = 'fill') +
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels = percent) + 
    labs(x = 'Embarked', y = 'Rate',
         title = 'Bar plot', subtitle = 'How many people survived in each Embarked')

# 5.4) Family Size
train %>%
    ggplot(aes(FamilySized, fill = Survived)) +
    geom_bar(position = 'fill') +
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels = percent) +
    labs(x = 'FamilySized', y = 'Rate',
         title = 'Bar plot', subtitle = 'Survival rate by FamilySized')
#=> 동승한 인원수에 따라 생존율에 차이가 있고 'FamilySized'와 'Survived'는 비선형 관계임을 알 수 있다.

# 5.5) Age.Group
dev.new()
train %>% 
    ggplot(aes(Age.Group, fill = Survived)) +
    geom_bar(postion = 'fill') +
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels = percent) +
    labs(x= 'Age group', y = 'Rate',
         title = 'Bar Plot', subtitle = 'Survival rate by Age group')

# 5.6)title 
train %>% 
    ggplot(aes(title, fill = Survived)) +
    geom_bar(position = 'fill') +
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels = percent) +
    labs(x = title, y = 'Rate',
         title = 'Bar plot', subtitle = 'Survival rate by passengers title')

# 5.7) ticket.size

train %>%
    ggplot(aes(ticket.size, fill = Survived)) +
    geom_bar(position =  'fill')+
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels = percent) +
    labs( x = 'ticket.size', y = 'Rate',
          title = 'Bar plot', subtitle = 'Survival rate by ticket.size')

# 5.8) Description of actual used features
# 생성한 파생변수 모두 유용, 실제 사용할 변수만 선택해 저장
 # (변수 설명)

    # | 변수명        | Type   | 설명                     | 
    # |:-------------:|:-------|:----------------------------------------------------------|
    # |**Survived**   | factor | Target feature, 생존 == 1, 사망 == 0 |
    # |**Sex**        | factor | 성별, `male` or `female` |
    # |**Pclass**     | factor | 선실 등급, 1등급(1), 2등급(2), 3등급(3) |
    # |**Embarked**   | factor | 승선항, 사우샘프턴(S), 셸부르(C), 퀸즈타운(Q) |
    # |**FamilySized**| factor | 가족의 규모, `SibSp`와 `Parch`를 이용해서 만든 파생변수, 범주는 3개     |
    # |**Age.Group**  | factor | 연령대, `Age`를 이용해서 만든 파생변수, 범주는 4개 |
    # |**title**      | factor | 이름의 일부분, `Name`을 이용해서 만든 파생변수, 범주는 5개 |
    # |**ticket.size**| factor | 티켓의 고유한 부분의 길이, `ticket`을 이용해서 만든 파생변수, 범주는 3개 |
    

# 1) Id Number 제외하고 실제 사용할 7개 입력변수와 1개의 타켓변수를 선택, 저장
train <- train%>%
    select('Pclass','Sex','Embarked', 'FamilySized',
           'Age.Group','title','ticket.size', 'Survived')
# 2) Submit 위해 Id벡터 추출해 ID에 저장
ID <- test$
# 3) Id와 Survived 제외한 나머지 6개 변수들 선택, 저장
test <- test %>%
    select('Pclass','Sex','Embarked', 'FamilySized',
           'Age.Group','title','ticket.size')

# 6. Machine learning model generation
# ['train' data set 이용해 기계학습 모델 생성]
#  원래는 'train', 'validation', 'test' data set 먼저 만들고 다양한 모델들 생성한 후 
# 교차검증(CV)을 거쳐 최종 모델을 선택하는 것이 맞지만 생략해 'RandomForset' 생성한 후 'test' data를 예측해보고
# 'competition'에 submit할 데이터 생성

# 6.1) Random Forest model generation
 # - 재현성 위해 seed number생성
titanic.rf <- randomForest(Survived ~., data = train, importance = T, ntree = 2000)

# 6.2) Feature importance check
importance(titanic.rf)
varImpPlot(titanic.rf)
## MDG : Mean Decrease Gini
 - 각 나무가 가지를 뻗어나갈 때마다 선택되는 변수들의 불순도 감소량을 측정해 전체 나무로부터 그 평균치 값을 사용한다.
 - MDG 값이 크다 -> 동일 범주끼리 묶이도록 하는데 일조(그 변수를 가지고 개체들을 분류하게 되면 불순도를 감소시킨다.)

## MDA : Mean Decrease Accuracy 
 - 구축된 나무의 정확도가 특정 변수 제거 후 재구축시 감소되는 정확도의 차이를 변수별로 평균화한 값
 - 분류 정확도를 높이는데 큰 영향을 준 변수일수록 그 변수를 제러했을 때 정확도의 감소량은 커진다.

=> 랜덤포레스트의 변수 중요도를 측정하는 두 지표는 값이 커질수록 변수의 중요도가 높아진다.
# 6.3) Predict test data and create submit data
# Predict
pred.rf <- predict(object = titanic.rf, newdata = test, type = 'Class')

# data Frame generation
submit <- data.frame(passengerID = ID, Survived = pred.rf)

# Write the submit data frame to file : setwd()로 지정한 폴더에 csv파일로 생성
write.csv(submit, file = './titanic_submit.csv', row.names = F)

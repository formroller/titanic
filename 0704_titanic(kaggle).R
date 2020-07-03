
getwd()
setwd('../titanic')
## 2.1 packages
```{r message = FALsE, warning = FALSE}

install.packages(c('readr','descr', 'VIM','ggplot2','RColorBrewer', 'scales', 'tidyversr','dplyr','purrr','tidyr','randomForest','caret','ROCR'))

# data input, assesment
library(readr)  # data input with readr :: read_csv()
library(descr)  # descr::CrossTable() - ���ֺ� �󵵼�, ������ ��ġ�� Ȯ��

# visualization
library(VIM)            # missing vlaues assesment used by VIM::aggr()
library(ggplot2)        # Used in almost visualization
library(RColorBrewer)   # plot color
library(scales)         # plot setting - x/y�� ����

# Feature engineering, Data Pre-Processing
#library(tidyversr)
library(dplyr)   # feature Engineering & Data Pre-Processing
library(purrr)   # Check missing values
library(tidyr)   # tidyr::gater() //https://gomguard.tistory.com/229

# Model generation
library(randomForest)
```



# �� ȭ�鿡 �������� plot ���
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



##2.2 raw data impory : ���������� �ҷ�����
train <- readr::read_csv('../titanic/train.csv')
test <- readr::read_csv('../titanic/test.csv')

 # readr::read_csv() : read.csv()���� �޸𸮿� �� ���� �ø� �� �ִ�.
 #                   chr / factor ���� ���ϰ� chr�Ӽ����� ���� 

full<- dplyr::bind_rows(train,test)

 # dplyr::bind_rows : rbind()�� ����(dimension)�� �����ʾ� ���պҰ� 
 #                  : test�� Survived�� NA�� ó���ϸ� �ϳ��� ����

##2.3 ���� �ǹ�
| ������        | �ؼ�(�ǹ�)                       | Type      | 
    |:-------------:|:---------------------------------|:----------|
    |**PassengerID**|�°��� �����ϴ� ���� ID number    |Int        |
    |**Survived**   |�°��� ���� ���θ� ��Ÿ���� ������ 1, ����� 0 �Դϴ�.|Factor|
    |**Pclass**     |������ ������μ� 1���(1)���� 3���(3)���� 3�� �����Դϴ�.|Ord.Factor|
    |**Name**       |�°��� �̸�                       |Factor|
    |**Sex**        |�°��� ����                       |Factor|
    |**Age**        |�°��� ����                       |Numeric|
    |**SibSp**      |�� �°��� �����ϴ� ���� �Ǵ� ������� ���� �����ϴ� �����̸� 0���� 8���� �����մϴ�.                            |Integer|
    |**Parch**      |�� �°��� �����ϴ� �θ�� �Ǵ� �ڳ��� ���� �����ϴ� �����̸� 0���� 9���� �����մϴ�.                            |Integer|
    |**Ticket**     |�°��� ž���� Ƽ�Ͽ� ���� ���ڿ� ����|Factor|
    |**Fare**       |�°��� ���ݱ��� �����ϸ鼭 ������ �ݾ׿� ���� ����|Numeric|
    |**Cabin**      |�� �°��� ������ �����ϴ� �����̸� ���ֿ� ����ġ�� �ʹ� �����ϴ�.                                          |Factor|
    |**Embarked**   |�¼���, �������� ��Ÿ���� C, Q, S 3�� �����̴�.|Factor|


##2.4 Change the variable type : ���� �Ӽ� ��ȯ

# EDA�� feature engineering�� �ռ� ���� �Ӽ� ��ȯ
# data���� input�ϸ鼭 `full`���� `factor` �Ӽ��� �������� ���⵵ �ϰ� 
# `Pclass`�� 1, 2, 3�� 1���, 2���, 3����� ��Ÿ���� `factor`�̱� �����Դϴ�.

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
# dat������ ����ġ Ȥ�� �̻�ġ �����ϴ��� ���� ���� �����Ϳ��� Ž���ϰ� �����ϴ� ����

##3.1 ��ġ���� Ȱ���� data Ȯ��
head(), summary()�� �Լ����� �ᱣ��(Output)�� ���� data Ȯ��

##3.1.1 head()
```{r}
head(full, 10)
```
 # Age - NA ����

##3.1.2 str()
str(full)

##3.1.3 summary()
summary(full)

1. Survived : (target), 418(missing values)�� test data
2. Pclass   : 1,2,3������� �������̸�, 3����� ���� ����
3. Name     : �̸��� ����� �°� ���� -> ������ ���� ž���� �°� ���������� ����
4. Sex      : ������ �������� 2�� ����.
5. Age      : 0.17 - 80������ ����/ 0.17�� ������ or �̻�ġ���� Ȯ�� �ʿ� / 263���� ����ġ
6. Sibsp    : 0-8, 3������ : 1, �κ� �Ǵ� ������ �Բ� ž�������� ���� ����
7. Parch    : 0-9, 3������ : 0, �θ� �Ǵ� �ڳ�� �Բ� ž���� �°� ���� ����.
8. Ticket   : 3.1.2str() - ���� ����, �Ϻ� ����, ���� �ٸ� => Ticket Size�� �Ļ����� ����
9. Fare     : 0 - 512/ 1���� ����ġ / 3���� : 31.275 / �̻� : 512
10.Cabin    : ���� ���� ����ġ(1014)/ ���� ���� Ż�� ����
11.Embarked : 3���� ���ַ� ���� / S�� ���� ���� / 2���� ����ġ

##3.2 Missing Values
#1) vim
#2) tidyverse - ������ ����ġ ���� ����
����ġ�� �ִ� ���� Ȯ���ϸ� �󸶳� �����ϴ��� Ȯ��

##3.2.1 VIM Pacakages
dev.new()
VIM::aggr(full, prop = FALSE, combined = T, numbers = T, sortVars = T, sortCombs = T)

##3.2.2 tidyverse pakages

full %>%
    dplyr::summarize_all(funs(sum(is.na(.))/n()))

#1) feature�� ����ġ ���� ��� ->  Data Frame �Ӽ�(1 * 12)
missing_values <- full %>% 
    dplyr::summarize_all(funs(sum(is.na(.))/n()))

#2) �� missing_values -> 12*2 data frame��  ����    
missing_values <-tidyr::gather(missing_values, key = 'feature', value = 'missing_pct')

#3) missing_values �̿��� �ð�ȭ
missing_values %>% 
    #Aesthetic settinfg : Missing_pct ������������ ����
    ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
    #Bar Plot
    geom_bar(stat = 'identity', fill = 'red') +
    #Title generation
    ggtitle('Rate of missing values in each features')+
    #Title detail setting
    theme(plot.title = element_text(face = 'bold', # �۾�ü
                                    hjust = 0.5,   # Horizon(���κ���)
                                    size = 15, color = 'darkblue')) +
    #x, y axis label setting 
    labs(x = 'Feature names', y = 'rate') +
    #plot�� x,y�� ��ȯ
    coord_flip()
 # => feature�� ����ġ ���� Ȯ�� ����
 # => purrr��Ű���� ���� ����ġ ���� ��� ��, ����ġ �����ϴ� ������ ���� �� �ð�ȭ
purrr::map_dbl(full, function(x){round((sum(is.na(x))/length(x)) * 100,1) })

# 1) ������ ����ġ ���� ���
miss_pct <- purrr::map_dbl(full, function(x){round((sum(is.na(x))/length(x)) * 100, 1) })

# 2) ����ġ ������ 0%���� ū ������ ����
miss_pct <- miss_pct[miss_pct >0]

# 3)Data Frame ����
data.frame(miss = miss_pct, var = names(miss_pct), row.names = NULL) %>%
    # Aesthetic setting : miss ������������ ����
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
    #plot�� x,y�� ��ȯ
    coord_flip()

 # => 12���� ������ 4�� �������� ����ġ ����('Survived'�� 'test' data �̹Ƿ� ����) 'Cabin', 'Age', 'Embarked', 'Fare'

## �ð�ȭ ���� feature�� �м�, Ž�� ����

#3.3 Age

dev.new()
age.p1 <- full %>%
    ggplot(aes(Age)) + 
    # ������׷� �׸���, ����
    geom_histogram(breaks = seq(0, 80, by = 1),  # ���� ����
                   col = 'red',                  # ���� ��輱 ����
                   fill = 'green',               # ���� ���� ����
                   alpha = .5) +                 # ���� ������ = 50% 
    #Plor title
    ggtitle ('All Titanic passengers age histogram') +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5,
                                    size = 15, color = 'darkblue'))
age.p2 <- full %>%
    # test data set �� Sruvived == NA �� �� ���� 
    filter(!is.na(Survived)) %>%
    ggplot(aes(Age, fill = Survived)) +
    geom_density(alpha = .5)+
    ggtitle('Titanic passengers age density plot') +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5,
                                    size = 15, color = 'darkblue'))

# multiplot.layout ���� ����
multi.layout = matrix(c(1,1,2,2), 2,2, byrow = T)

# ������ ������ 2���� �׷��� �� ȭ�鿡 ���
multiplot(age.p1, age.p2, layout = multi.layout)


##3.4 Pclass
# Pclass�� �ش��ϴ� ž�°��� �󵵼� �ð�ȭ
# - dplyr ��Ű�� Ȱ��, Pclass���� �׷�ȭ �� �� ���ֺ� �󵵼� ��Ÿ���� Data Frame ���� , ggplot���� �ð�ȭ

full %>% 
    #dplyr::group_by(), summarize() �̿��� Pclass �󵵼� ���ϱ�
    group_by(Pclass) %>%
    summarize(N = n()) %>%
    # Aesthetic setting
    ggplot(aes(Pclass, N))+
    geom_col() +
    # Pclass �󵵼� plot�� ���
    geom_text(aes(label = N),      # Plot�� y�� �ش��ϴ� N(�󵵼�)�� ����
              size = 5,            # �۾� ũ��
              vjust = 1.2,         # vertical(����) ��ġ ����
              color = '#ffffff') + # �۾� �� : ���
    # Plot title
    ggtitle('Number of each Pclass\'s passengers') + 
    # Title setting
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15)) +
    #x, y axis name change
    labs(x ='Pclass', y = 'Count')

 # => 3��� ���ǿ� ž���� �°��� ���� ����
 # ���� ��޺� �������� ���� �ٷ�

##3.5 Fare
# Fare�� ���� �ð�ȭ(������׷�, ���ڱ׸�)

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
    # ����ġ�� ȸ��������, �ߺ��Ǵ� �κ��� ������ ���
    geom_jitter(col = 'grey') +
    # ���ڱ׸� ������ : 50%
    geom_boxplot(alpha = .5) +
    ggtitle('Boxplot of passengers Fare')+
    theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 15))

#multiplot layout ���� ����
multi.layout = matrix(c(1,1,2,2),2,2)
# ������ ������ �׷��� �� ȭ�鿡 ���
dev.new()
multiplot(Fare.p1, Fare.p2, layout = multi.layout)

# => �����ڵ��� ����� �°��麸�� Fare ������ ū ���̴� ����
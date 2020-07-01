install.packages(c('stringr','dplyr'))
install.packages('psych')
install.packages('plyr')
install.packages('ggplot2')

library(stringr)
library(dplyr)
library(psych)
library(plyr)
library(ggplot)
getwd()
setwd('../titanic')


train <- read.csv('train.csv', row.names = 'PassengerId')
test <- read.csv('test.csv', row.names = 'PassengerId')
sub <- read.csv('gender_submission.csv')

head(train)
head(test)

str(train)

colnames(train)
colnames(test)



# X값 컬럼별 가공, 
# na처리, 이름(호칭) 정리, 성별 더미화
# 1) na
colSums(is.na(train))  # age 177
describe(train$Age)

# ifelse
train$new_age<- ifelse(train$Age <10,0,
       ifelse(train$Age<20,10,
              ifelse(train$Age<30, 20,
                     ifelse(train$Age<40,30,
                            ifelse(train$Age<50,40,
                                   ifelse(train$Age<60,50,
                                          ifelse(train$Age<70,60,
                                                 ifelse(train$Age<80,70,
                                                        ifelse(train$Age<90,80)))))))))

# for
ages<- c()
for (i in train$Age){
    if (i <10){
        ages<-0
    }else if(i<20) {
        ages<-10
    }else if(i<30){
        ages<-20
    }
}

dev.new()
barplot(train$new_age, ylim = c(0,90))

barplot(seq(0,90,10),train$new_age,)
count(train$new_age)


# 2) split name
str_split("Graham, Miss. Margaret Edith",'. ')[[1]][2]

 f_split <- function(x, sep ='. ', ord = 1) {
     str_split(x, sep)[[1]][ord]
 }
 
sapply(train$Name, f_split, '. ',1)
train$alias <- sapply(train$Name, f_split, '. ',2)

head(train) 
unique(alias)
count(train$alias)

plot(train$alias)

# gender(성별 더미화)
X$Sex
X$Gender <- as.factor(ifelse(X$Sex=='male',0,1))
dev.new()
plot(X$Gender)

##age

dev.new()
plot(formula = Survived ~ Age, data = train)

unique(X$Age)
for (i in 1:nrow(X)){
    if (i < 10){
        str_replace_all(i, 0)
    }ifelse 
}


###modeling

## train -> X, y split
X <- train[colnames(train)!= 'Survived']
y <- data.frame(as.factor(train$Survived))
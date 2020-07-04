adult <- read.csv('adult_salary.csv')
head(adult)
str(adult)
any(is.na(adult))
print(summary(adult))
library(dplyr)
adult <- select(adult,-X)
head(adult)
getwd()

# Data Cleaning 

# Employer Column
table(adult$type_employer)

# Combining two small groups into a single group called "Unemployed"
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

#Combining State and Local gov jobs into SL-gov and self-employed jobs into self-emp
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,group_emp)
table(adult$type_employer)

# Martial Column
table(adult$marital)
group_marital <- function(mar){
mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

# Country Column 
table(adult$country)
levels(adult$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)

# Using table for confirming grouping 
table(adult$country)

# Confirming the changes in the structure 
str(adult)
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
str(adult)

# Missing Data 
library(Amelia)

# Converting missing item to NA
adult[adult == '?'] <- NA

# Confirming the missing values as 0 
table(adult$type_employer)

# Conveting variables into factor
adult$type_employer <- factor(adult$type_employer)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)
adult$occupation <- factor(adult$occupation)

# For checking the missing values with the help of map fuction 
missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','peachpuff'))

# Omiting the missing values 
adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('red','peachpuff'))

### Exploratory Data Analysis 
library(ggplot2)
library(dplyr)
str(adult)

# Creating a histogram of ages
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='white',binwidth=1) + theme_bw()

# Ploting a histogram of hours worked per week
ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(color='red'),bins = 30) + theme_bw()

# Creating a barplot of country
ggplot(adult,aes(native.country)) + geom_bar(aes(fill=income))+theme_bw()

# Building a model 
library(caTools)

# checking before building a model 
head(adult)
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) 

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

##Logistic model
model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

# Confusion Matrix and predictions
test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)

# accuracy 
(6372+1423)/(6372+1423+548+872)

#Specificity
(1423/(1423+872))

##recall and sensitivity
6732/(6372+548)

#precision
6732/(6372+872)

#miss-classification
(548+872)/(6372+1423+548+872)

# SUPPORT VECTOR MACHINE 

library(ISLR)
library(e1071)

# checking before building a model 
head(adult)
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample.adult <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train.adult = subset(adult, sample.adult == TRUE)

# Testing Data
test.adult = subset(adult, sample.adult == FALSE)

# Applying SVM Model
model.adult = svm(income ~ .,data = train.adult)
summary(model.adult)

# Prediction of data and Confusion Matrix
test.adult$pred.value = predict(model.adult, newdata=test.adult, type="response")
table(test.adult$income, test.adult$pred.value)

# accuracy 
(6482+1332)/(6482+1332+438+963)

##recall and sensitivity
6482/(6482+438)

#Specificity
1332/(1332+963)

#precision
6482/(6482+963)

# miss-classification
(438+963)/(6482+1332+438+963)

# Removing outliers 
boxplot(adult$capital_loss)$out
outliers <- boxplot(adult$capital_loss, plot=FALSE)$out
adult <- adult[-which(adult$capital_loss %in% outliers),]

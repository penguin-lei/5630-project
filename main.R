source("functions.R")
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(caret)
library(MASS)

# read data ------------------------
math = read.csv("student-mat.csv", sep = ';', stringsAsFactors = T)
str(math)
por = read.csv("student-por.csv", sep = ";", stringsAsFactors = T)
str(por)

#set dummies
math = fastDummies::dummy_columns(math, remove_first_dummy = T, remove_selected_columns = T)
por = fastDummies::dummy_columns(por, remove_first_dummy = T, remove_selected_columns = T)


predictors = setdiff(colnames(math), c("G1", "G2", "G3"))

math$grade.cat = cut(math$G1, breaks=c(-1, 9, 11, 13, 15, 20),
                     labels=c('F', 'D', 'C', 'B', 'A'))
por$grade.cat = cut(por$G1, breaks=c(-1, 9, 11, 13, 15, 20),
                    labels=c('F', 'D', 'C', 'B', 'A'))
por$grade.con = por$G1
math$grade.con = math$G1


# split data -------------------------
set.seed(114514)
train_idx.math = sample(1:nrow(math), floor(nrow(math))*0.8, replace = F)
train_idx.por = sample(1:nrow(por), floor(nrow(por))*0.8, replace = F)

math.train = math[train_idx.math,]
math.test = math[-train_idx.math,]

por.train = por[train_idx.por,]
por.test = por[-train_idx.por,]

preproc.param <- math.train %>%
  preProcess(method = c("center", "scale"))
math.train <- preproc.param %>% predict(math.train)
math.test <- preproc.param %>% predict(math.test)
preproc.param <- por.train %>%
  preProcess(method = c("center", "scale"))
por.train <- preproc.param %>% predict(por.train)
por.test <- preproc.param %>% predict(por.test)
# EDA --------------------------------
response_variable = ''
# descriptive(sheet_name = '', data = math.train, descriptive_variables = colnames(math.train))

math.grade.train = math.train[,c("G1","G2","G3")]
por.grade.train = por.train[,c("G1", "G2", "G3")]

library(PerformanceAnalytics)

chart.Correlation(math.grade.train, histogram = TRUE, method = "pearson")
chart.Correlation(por.grade.train, histogram = TRUE, method = "pearson")



# Classification ---------------------------------
## svm -----------------------------
library(e1071)
classification.formula = as.formula(paste("grade.cat~", paste(predictors, collapse = "+"), sep = ''))
regression.formula = as.formula(paste("grade.con~", paste(predictors, collapse = "+"), sep = ''))

math.svm.fit = svm(classification.formula, data = math.train)
math.svm.predict = predict(math.svm.fit, math.test)

### svm cv ----------------------
set.seed(114514)
# svm.cv.mine(q3.train = math.train, form = classification.formula, k = 10, response_variable = "grade.cat")


set.seed(114514)
cost.list = exp(seq(from = -6, to = 2, length.out = 100))
svm.cv.val.error = svm.cv.cost(q3.train = math.train, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = svm.cv.val.error))

math.svm.fit = svm(classification.formula, data = math.train, cost = cost.list[which.min(svm.cv.val.error)], kernel = 'linear')

## logistic regression
math.glm<-glm(classification.formula, data = math.train, family = "binomial")
predict(math.glm,math.test,type="response")



## logistic regression lasso------

### l-r LASSO math--------
set.seed(114514)
math.logistic.lasso.cv = cv.glmnet(x = as.matrix(math.train[,predictors]), y = math.train$grade.cat, family = "multinomial", 
                                   alpha = 1, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
plot(math.logistic.lasso.cv)
math.logistic.lasso.best = glmnet(x = as.matrix(math.train[,predictors]), y = math.train$grade.cat, family = "multinomial", 
                                  alpha = 1, lambda = math.logistic.lasso.cv$lambda.min, standardize = TRUE)

#### l-r LASSO math classification-------
math.logistic.lasso<-cbind(math.test$grade.cat,predict(math.logistic.lasso.best,as.matrix(math.test[,predictors]),type="class"))
colnames(math.logistic.lasso)<-c("true","predicted")
View(math.logistic.lasso)

#### l-r LASSO math Misclassification rate
1-mean(math.test$grade.cat==predict(math.logistic.lasso.best,as.matrix(math.test[,predictors]),type="class"))

### l-r LASSO Portuguese-------
set.seed(114514)
por.logistic.lasso.cv = cv.glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                  alpha = 1, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
plot(por.logistic.lasso.cv)
por.logistic.lasso.best = glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                 alpha = 1, lambda = por.logistic.lasso.cv$lambda.min, standardize = TRUE)

#### l-r LASSO Portuguese classification-------
por.logistic.lasso<-cbind(por.test$grade.cat,predict(por.logistic.lasso.best,as.matrix(por.test[,predictors]),type="class"))
colnames(por.logistic.lasso)<-c("true","predicted")
View(por.logistic.lasso)

#### l-r LASSO Portuguese Misclassification rate
1-mean(por.test$grade.cat==predict(por.logistic.lasso.best,as.matrix(por.test[,predictors]),type="class"))

## logistic regression ridge--------

### l-r ridge math-------
set.seed(114514)
math.logistic.ridge.cv = cv.glmnet(x = as.matrix(math.train[,predictors]), y = math.train$grade.cat, family = "multinomial", 
                                   alpha = 0, lambda = exp(seq(from = -5, to = 2, length.out = 100)), standardize = TRUE)
plot(math.logistic.ridge.cv)
math.logistic.ridge.best = glmnet(x = as.matrix(math.train[,predictors]), y = math.train$grade.cat, family = "multinomial", 
                                  alpha = 0, lambda = math.logistic.ridge.cv$lambda.min, standardize = TRUE)


#### l-r ridge math classification-------
math.logistic.ridge<-cbind(math.test$grade.cat,predict(math.logistic.ridge.best,as.matrix(math.test[,predictors]),type="class"))
colnames(math.logistic.ridge)<-c("true","predicted")
View(math.logistic.ridge)

#### l-r ridge math Misclassification rate-------
1-mean(math.test$grade.cat==predict(math.logistic.ridge.best,as.matrix(math.test[,predictors]),type="class"))

### l-r ridge Portuguese-------
set.seed(114514)
por.logistic.ridge.cv = cv.glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                  alpha = 0, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
plot(por.logistic.ridge.cv)
por.logistic.ridge.best = glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                 alpha = 0, lambda = por.logistic.ridge.cv$lambda.min, standardize = TRUE)

#### l-r ridge Portuguese classification-------
por.logistic.ridge<-cbind(por.test$grade.cat,predict(por.logistic.ridge.best,as.matrix(por.test[,predictors]),type="class"))
colnames(por.logistic.ridge)<-c("true","predicted")
View(por.logistic.ridge)

#### l-r ridge Portuguese Misclassification rate-------
1-mean(por.test$grade.cat==predict(por.logistic.ridge.best,as.matrix(por.test[,predictors]),type="class"))

## LDA -----------------------------------------
### LDA math------------
math.lda<-lda(classification.formula,data=math.train)
math.coord.1<-as.matrix(math.train[,predictors]) %*% math.lda$scaling[,1]
math.coord.2<-as.matrix(math.train[,predictors]) %*% math.lda$scaling[,2]
ggplot(data.frame(math.coord.1,math.coord.2,math.train$grade.cat))+
  geom_point(aes(math.coord.1, math.coord.2, color = math.train$grade.cat))

#### LDA math classification-------
LDA.math.pred<-cbind(math.test$grade.cat,predict(math.lda,newdata=math.test)$class)
colnames(LDA.math.pred)<-c("true","predicted")
View(LDA.math.pred)

#### LDA math Misclassification Rate------
1-mean(math.test$grade.cat==predict(math.lda,newdata=math.test)$class)

### LDA Portuguese------------
por.lda<-lda(classification.formula,data=por.train)
por.coord.1<-as.matrix(por.train[,predictors]) %*% por.lda$scaling[,1]
por.coord.2<-as.matrix(por.train[,predictors]) %*% por.lda$scaling[,2]
ggplot(data.frame(por.coord.1,por.coord.2,por.train$grade.cat))+
  geom_point(aes(por.coord.1, por.coord.2, color = por.train$grade.cat))


#### LDA Portuguese classification-------
LDA.por.pred<-cbind(por.test$grade.cat,predict(por.lda,newdata=por.test)$class)
colnames(LDA.por.pred)<-c("true","predicted")
View(LDA.por.pred)

#### LDA Portuguese Misclassification Rate--------
1-mean(por.test$grade.cat==predict(por.lda,newdata=por.test)$class)


# Regression -------------------------------------

## OLS -------------------------------------------
###OLS math-------
math.ols<-lm(regression.formula,data=por.train)

#### OLS math prediction------
math.ols.pred<-(cbind(math.test$grade.con,predict(math.ols,math.test)))
colnames(math.ols.pred)<-c("true","predicted")
View(math.ols.pred)

#### OLS math Mean Squared Error------
mean((math.test$grade.con-predict(math.ols,newdata=math.test[,predictors]))^2)

###OLS Portuguese--------
por.ols<-lm(regression.formula,data=por.train)

#### OLS Portuguese prediction------
por.ols.pred<-(cbind(por.test$grade.con,predict(por.ols,por.test)))
colnames(por.ols.pred)<-c("true","predicted")
View(por.ols.pred)

#### OLS Portuguese Mean Squared Error-------
mean((por.test$grade.con-predict(por.ols,newdata=por.test[,predictors]))^2)

## Ridge -----------------------------------------
### ridge math------------
set.seed(114514)
math.ridge.cv<-cv.glmnet(as.matrix(math.train[,predictors]),math.train$grade.con,
                         alpha=0,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(math.ridge.cv)
math.ridge.best<-glmnet(math.train[,predictors],math.train$grade.con,
                        alpha=0,lambda = math.ridge.cv$lambda.min,standardize=TRUE)

#### ridge math prediction------
math.ridge.pred<-(cbind(math.test$grade.con,predict(math.ridge.best,as.matrix(math.test[,predictors]))))
colnames(math.lasso.pred)<-c("true","predicted")
View(math.ridge.pred)

#### ridge math Mean Squared Error--------
mean((math.test$grade.con-predict(math.ridge.best,as.matrix(math.test[,predictors])))^2) 

### ridge Portuguese-------------

set.seed(114514)
por.ridge.cv<-cv.glmnet(as.matrix(por.train[,predictors]),por.train$grade.con,
                         alpha=0,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(por.ridge.cv)
por.ridge.best<-glmnet(por.train[,predictors],por.train$grade.con,
                        alpha=0,lambda = por.ridge.cv$lambda.min,standardize=TRUE)

#### ridge Portuguese prediction------
por.ridge.pred<-(cbind(por.test$grade.con,predict(por.ridge.best,as.matrix(por.test[,predictors]))))
colnames(math.lasso.pred)<-c("true","predicted")
View(por.ridge.pred)

#### ridge Portuguese Mean Squared Error--------
mean((por.test$grade.con-predict(por.ridge.best,as.matrix(por.test[,predictors])))^2)

## LASSO -----------------------------------------
### LASSO math----------
set.seed(114514)
math.lasso.cv<-cv.glmnet(as.matrix(math.train[,predictors]),math.train$grade.con,
                         alpha=1,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(math.lasso.cv)
math.lasso.best<-glmnet(math.train[,predictors],math.train$grade.con,
                        alpha=1,lambda = math.lasso.cv$lambda.min,standardize=TRUE)

#### LASSO math prediction------
math.lasso.pred<-(cbind(math.test$grade.con,predict(math.lasso.best,as.matrix(math.test[,predictors]))))
colnames(math.lasso.pred)<-c("true","predicted")
View(math.lasso.pred)

#### LASSO math Mean Squared Error-------
mean((math.test$grade.con-predict(math.lasso.best,as.matrix(math.test[,predictors])))^2)

#### Math Important variable------
coef(math.lasso.best)

### LASSO Portuguese--------
set.seed(114514)
por.lasso.cv<-cv.glmnet(as.matrix(por.train[,predictors]),por.train$grade.con,
                        alpha=1,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(por.lasso.cv)
por.lasso.best<-glmnet(por.train[,predictors],por.train$grade.con,
                       alpha=1,lambda = por.lasso.cv$lambda.min,standardize=TRUE)

#### LASSO por prediction------
por.lasso.pred<-(cbind(por.test$grade.con,predict(por.lasso.best,as.matrix(por.test[,predictors]))))
colnames(math.lasso.pred)<-c("true","predicted")
View(por.lasso.pred)

#### LASSO  Portuguese Mean Squared Error--------
mean((por.test$grade.con-predict(por.lasso.best,as.matrix(por.test[,predictors])))^2)

#### Portuguese important variable------
coef(por.lasso.best)


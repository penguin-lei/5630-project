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

math.trian = math[train_idx.math,]
math.test = math[-train_idx.math,]

por.train = por[train_idx.por,]
por.test = por[-train_idx.por,]

preproc.param <- math.trian %>%
  preProcess(method = c("center", "scale"))
math.trian <- preproc.param %>% predict(math.trian)
math.test <- preproc.param %>% predict(math.test)
preproc.param <- por.train %>%
  preProcess(method = c("center", "scale"))
por.train <- preproc.param %>% predict(por.train)
por.test <- preproc.param %>% predict(math.test)
# EDA --------------------------------
response_variable = ''
# descriptive(sheet_name = '', data = math.trian, descriptive_variables = colnames(math.trian))

math.grade.train = math.trian[,c(31,32,33)]
por.grade.train = por.train[,c(31, 32, 33)]

library(PerformanceAnalytics)

chart.Correlation(math.grade.train, histogram = TRUE, method = "pearson")
chart.Correlation(por.grade.train, histogram = TRUE, method = "pearson")



# classification ---------------------------------
## svm -----------------------------
library(e1071)
classification.formula = as.formula(paste("grade.cat~", paste(predictors, collapse = "+"), sep = ''))
regression.formula = as.formula(paste("grade.con~", paste(predictors, collapse = "+"), sep = ''))

math.svm.fit = svm(classification.formula, data = math.trian)
math.svm.predict = predict(math.svm.fit, math.test)

### svm cv ----------------------
set.seed(114514)
# svm.cv.mine(q3.train = math.trian, form = classification.formula, k = 10, response_variable = "grade.cat")


set.seed(114514)
cost.list = exp(seq(from = -6, to = 2, length.out = 100))
svm.cv.val.error = svm.cv.cost(q3.train = math.trian, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = svm.cv.val.error))

math.svm.fit = svm(classification.formula, data = math.trian, cost = cost.list[which.min(svm.cv.val.error)], kernel = 'linear')

## logistic regression
math.glm<-glm(classification.formula, data = math.trian, family = "binomial")



## logistic regression lasso
#math
set.seed(114514)
math.logistic.lasso.cv = cv.glmnet(x = as.matrix(math.trian[,predictors]), y = math.trian$grade.cat, family = "multinomial", 
                                   alpha = 1, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
math.logistic.lasso.best = glmnet(x = as.matrix(math.trian[,predictors]), y = math.trian$grade.cat, family = "multinomial", 
                                  alpha = 1, lambda = math.logistic.lasso.cv$lambda.min, standardize = TRUE)
mean(math.test$grade.cat==predict(math.logistic.lasso.best,as.matrix(math.test[,predictors]),type="class"))
#por
set.seed(114514)
por.logistic.lasso.cv = cv.glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                  alpha = 1, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
por.logistic.lasso.best = glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                 alpha = 1, lambda = por.logistic.lasso.cv$lambda.min, standardize = TRUE)
mean(por.test$grade.cat==predict(por.logistic.lasso.best,as.matrix(por.test[,predictors]),type="class"))

## logistic regression ridge
#math
set.seed(114514)
math.logistic.ridge.cv = cv.glmnet(x = as.matrix(math.trian[,predictors]), y = math.trian$grade.cat, family = "multinomial", 
                                   alpha = 0, lambda = exp(seq(from = -5, to = 2, length.out = 100)), standardize = TRUE)
math.logistic.ridge.best = glmnet(x = as.matrix(math.trian[,predictors]), y = math.trian$grade.cat, family = "multinomial", 
                                  alpha = 0, lambda = math.logistic.ridge.cv$lambda.min, standardize = TRUE)
mean(math.test$grade.cat==predict(math.logistic.ridge.best,as.matrix(math.test[,predictors]),type="class"))
#por
set.seed(114514)
por.logistic.ridge.cv = cv.glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                  alpha = 0, lambda = exp(seq(from = -5, to = 2, length.out = 100)),standardize=TRUE)
por.logistic.ridge.best = glmnet(x = as.matrix(por.train[,predictors]), y = por.train$grade.cat, family = "multinomial", 
                                 alpha = 0, lambda = por.logistic.ridge.cv$lambda.min, standardize = TRUE)
mean(por.test$grade.cat==predict(por.logistic.ridge.best,as.matrix(por.test[,predictors]),type="class"))

## LDA -----------------------------------------
math.lda<-lda(classification.formula,data=math.trian)
math.coord.1<-as.matrix(math.trian[,predictors]) %*% math.lda$scaling[,1]
math.coord.2<-as.matrix(math.trian[,predictors]) %*% math.lda$scaling[,2]
ggplot(data.frame(coord.1,coord.2,math.trian$grade.cat))+
  geom_point(aes(math.coord.1, math.coord.2, color = math.trian$grade.cat))
mean(math.test$grade.cat==predict(math.lda,newdata=math.test)$class)

# Regression -------------------------------------

## OLS -------------------------------------------
#math
math.ols<-lm(regression.formula,data=por.train)
mean((math.test$grade.con-predict(math.ols,newdata=math.test[,predictors]))^2)
#por
por.ols<-lm(regression.formula,data=por.train)
mean((por.test$grade.con-predict(por.ols,newdata=por.test[,predictors]))^2)

## Ridge -----------------------------------------
#math
set.seed(114514)
math.ridge.cv<-cv.glmnet(as.matrix(math.trian[,predictors]),math.trian$grade.con,
                         alpha=0,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(math.ridge.cv)
math.ridge.best<-glmnet(math.trian[,predictors],math.trian$grade.con,
                        alpha=0,lambda = math.ridge.cv$lambda.min,standardize=TRUE)
mean((math.test$grade.con-predict(math.ridge.best,as.matrix(math.test[,predictors])))^2) 
#por
set.seed(114514)
por.ridge.cv<-cv.glmnet(as.matrix(por.train[,predictors]),por.train$grade.con,
                         alpha=0,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(por.ridge.cv)
por.ridge.best<-glmnet(por.train[,predictors],por.train$grade.con,
                        alpha=0,lambda = por.ridge.cv$lambda.min,standardize=TRUE)
mean((por.test$grade.con-predict(por.ridge.best,as.matrix(por.test[,predictors])))^2)

## LASSO -----------------------------------------
#math
set.seed(114514)
math.lasso.cv<-cv.glmnet(as.matrix(math.trian[,predictors]),math.trian$grade.con,
                         alpha=1,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(math.lasso.cv)
math.lasso.best<-glmnet(math.trian[,predictors],math.trian$grade.con,
                        alpha=1,lambda = math.lasso.cv$lambda.min,standardize=TRUE)
mean((math.test$grade.con-predict(math.lasso.best,as.matrix(math.test[,predictors])))^2)  
#por
set.seed(114514)
por.ridge.cv<-cv.glmnet(as.matrix(por.train[,predictors]),por.train$grade.con,
                        alpha=1,lambda = exp(seq(-10, 5, by=0.1)),standardize=TRUE)
plot(por.ridge.cv)
por.ridge.best<-glmnet(por.train[,predictors],por.train$grade.con,
                       alpha=1,lambda = por.ridge.cv$lambda.min,standardize=TRUE)
mean((por.test$grade.con-predict(por.ridge.best,as.matrix(por.test[,predictors])))^2)
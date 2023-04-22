source("functions.R")
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(caret)
library(MASS)

par(mar = c(6, 6, 4, 4))

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

classification.formula = as.formula(paste("grade.cat~", paste(predictors, collapse = "+"), sep = ''))
regression.formula = as.formula(paste("grade.con~", paste(predictors, collapse = "+"), sep = ''))
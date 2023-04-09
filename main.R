source("functions.R")


# ---------------------------- read data ------------------------
math = read.csv("student-mat.csv", sep = ';', stringsAsFactors = T)
str(math)
por = read.csv("student-por.csv", sep = ";", stringsAsFactors = T)
str(por)


predictors = setdiff(colnames(math), c("G1", "G2", "G3"))

math$grade.cat = cut(math$G1, breaks=c(0, 9, 11, 13, 15, 20),
                     labels=c('F', 'D', 'C', 'B', 'A'))
por$grade.cat = cut(por$G1, breaks=c(0, 9, 11, 13, 15, 20),
                    labels=c('F', 'D', 'C', 'B', 'A'))
por$grade.con = por$G1
math$grade.con = math$G1



# ---------------------------- split data -------------------------
set.seed(114514)
train_idx.math = sample(1:nrow(math), floor(nrow(math))*0.8, replace = F)
train_idx.por = sample(1:nrow(por), floor(nrow(por))*0.8, replace = F)

math.trian = math[train_idx.math,]
math.test = math[-train_idx.math,]

por.train = por[train_idx.por,]
por.test = por[-train_idx.por,]

# ----------------------------- EDA --------------------------------
response_variable = ''
# descriptive(sheet_name = '', data = math.trian, descriptive_variables = colnames(math.trian))

math.grade.train = math.trian[,c(31,32,33)]
por.grade.train = por.train[,c(31, 32, 33)]

library(PerformanceAnalytics)

chart.Correlation(math.grade.train, histogram = TRUE, method = "pearson")
chart.Correlation(por.grade.train, histogram = TRUE, method = "pearson")



# ---------------------------- classification ---------------------------------
### --------------------------- svm -----------------------------
library(e1071)
classification.formula = as.formula(paste("grade.cat~", paste(predictors, collapse = "+"), sep = ''))

math.svm.fit = svm(classification.formula, data = math.trian)
math.svm.predict = predict(math.svm.fit, math.test)

### -------------------------- svm cv ----------------------
set.seed(114514)
svm.cv.mine(q3.train = math.trian, form = classification.formula, k = 10, response_variable = "grade.cat")

# ---------------------------- Regression -------------------------------------
## --------------------------- OLS -------------------------------------------
## --------------------------- Ridge -----------------------------------------
## --------------------------- LASSO -----------------------------------------
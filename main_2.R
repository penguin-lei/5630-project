# baseline --------------------
math.con.base = mean(math.train$grade.con)
math.base.con.error = mean((math.test$grade.con - math.con.base)^2)
math.base.con.error.train = mean((math.train$grade.con - math.con.base)^2)

set.seed(114514)
math.cat.base = sample(names(table(math.train$grade.cat)/nrow(math.train)), nrow(math.test), table(math.train$grade.cat)/nrow(math.train), replace = T)
math.base.cat.error = mean(math.test$grade.cat != math.cat.base)
math.base.cat.error.train = mean(math.train$grade.cat != sample(names(table(math.train$grade.cat)/nrow(math.train)), nrow(math.train), table(math.train$grade.cat)/nrow(math.train), replace = T))




### svm cv ----------------------
set.seed(114514)
# svm.cv.mine(q3.train = por.train, form = classification.formula, k = 10, response_variable = "grade.cat")


set.seed(114514)
cost.list = exp(seq(from = -6, to = 2, length.out = 100))
por.svm.cv.val.error = svm.cv.cost(q3.train = por.train, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = por.svm.cv.val.error)) + labs(title = "Por svm")

por.svm.fit = svm(classification.formula, data = por.train, cost = cost.list[which.min(por.svm.cv.val.error)], kernel = 'linear')
por.svm.predict = predict(por.svm.fit, por.test,decision.values = F)
por.svm.test.error = mean(por.svm.predict != por.test$grade.cat)

set.seed(114514)
svm.cv.val.error = svm.cv.cost(q3.train = math.train, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = svm.cv.val.error)) + labs(title = "Math svm")

math.svm.fit = svm(classification.formula, data = math.train, cost = cost.list[which.min(svm.cv.val.error)], kernel = 'linear')
math.svm.predict = predict(math.svm.fit, math.test, decision.values = F)
math.svm.test.error = mean(math.svm.predict != math.test$grade.cat)


## logistic regression ------------------
library(nnet)

math.logistic.fit = multinom(formula = classification.formula, data = math.train, Hess = T)
predict(math.logistic.fit)
summary(math.logistic.fit)
math.logistic.error.train = mean(math.train$grade.cat != predict(math.logistic.fit))
math.logistic.error.test = mean(math.test$grade.cat != predict(math.logistic.fit, math.test))

por.logistic.fit = multinom(formula = classification.formula, data = por.train, Hess = T)
predict(por.logistic.fit)
summary(por.logistic.fit)
por.logistic.error.train = mean(por.train$grade.cat != predict(por.logistic.fit))
por.logistic.error.test = mean(por.test$grade.cat != predict(por.logistic.fit, por.test))


### svm cv ----------------------
set.seed(114514)
# svm.cv.mine(q3.train = por.trian, form = classification.formula, k = 10, response_variable = "grade.cat")


set.seed(114514)
cost.list = exp(seq(from = -6, to = 2, length.out = 100))
por.svm.cv.val.error = svm.cv.cost(q3.train = por.train, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = por.svm.cv.val.error)) + labs(title = "Por svm")

por.svm.fit = svm(classification.formula, data = por.trian, cost = cost.list[which.min(por.svm.cv.val.error)], kernel = 'linear')
por.svm.predict = predict(por.svm.fit, por.test,decision.values = F)
por.svm.test.error = mean(por.svm.predict != por.test$grade.cat)


svm.cv.val.error = svm.cv.cost(q3.train = math.trian, form = classification.formula, k = 10, response_variable = "grade.cat", cost.list = cost.list)
ggplot() + geom_line(aes(x = log(cost.list), y = svm.cv.val.error)) + labs(title = "Math svm")

math.svm.fit = svm(classification.formula, data = math.trian, cost = cost.list[which.min(svm.cv.val.error)], kernel = 'linear')
math.svm.predict = predict(math.svm.fit, math.test, decision.values = F)
math.svm.test.error = mean(math.svm.predict != math.test$grade.cat)

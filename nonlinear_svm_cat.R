svm.por.cat = svm.cv.mine.cat(por.train, classification.formula, 10, "grade.cat")
svm.por.cat
# $kernel
# [1] "radial"
# 
# $gamma
# [1] 0.002120408
# 
# $cost
# [1] 3.4

svm.por.cat.fit = do.call(svm, c(list(formula = classification.formula, data = por.train), svm.por.cat))
predict.test = predict(svm.por.cat.fit, por.test, decision.values = F)

svm.por.cat.error.test = mean((predict.test != por.test$grade.cat))

svm.math.cat = svm.cv.mine.cat(math.train, classification.formula, 10, "grade.cat")
svm.math.cat
# $kernel
# [1] "radial"
# 
# $gamma
# [1] 0.00252449
# 
# $cost
# [1] 5

svm.math.cat.fit = do.call(svm, c(list(formula = classification.formula, data = math.train), svm.math.cat))
predict.test = predict(svm.math.cat.fit, math.test, decision.values = F)

svm.math.cat.error.test = mean((predict.test != math.test$grade.cat))

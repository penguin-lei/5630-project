library(e1071)

## kernel svm -----------------------

svm.math.reg = svm.cv.mine(math.train, regression.formula, 10, "grade.con")
svm.math.reg
# $kernel
# [1] "radial"
# 
# $gamma
# [1] 0.01
# 
# $cost
# [1] 0.5

svm.math.reg.fit = do.call(svm, c(list(formula = regression.formula, data = math.train), svm.math.reg))
predict.test = predict(svm.math.reg.fit, math.test, decision.values = F)

svm.math.reg.error.test = mean((predict.test - math.test$grade.con)^2)

## math regression -------------------------
svm.por.reg = svm.cv.mine(por.train, regression.formula, 10, "grade.con")
svm.por.reg
# $kernel
# [1] "radial"
# 
# $gamma
# [1] 0.005959184
# 
# $cost
# [1] 0.8

svm.por.reg.fit = do.call(svm, c(list(formula = regression.formula, data = por.train), svm.por.reg))
predict.test = predict(svm.por.reg.fit, por.test, decision.values = F)

svm.por.reg.error.test = mean((predict.test - por.test$grade.con)^2)
svm.por.reg.error.test

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
svm.math.reg.error.train = mean((predict(svm.math.reg.fit) - math.train$grade.con)^2)
svm.math.reg.error.test

svm.math.reg.var.imp = numeric(length(predictors))
for(i in 1:length(predictors))
{
  svm.math.reg.fit.inf = do.call(svm, c(list(formula = as.formula(paste("grade.con~", paste(predictors[-i], collapse = "+"), sep = '')), data = math.train), svm.math.reg))

  svm.math.reg.var.imp[i] = mean((predict(svm.math.reg.fit.inf) - math.train$grade.con)^2) - svm.math.reg.error.train
}

svm.math.reg.var.imp = data.frame(predictors = predictors,
                                  importance = svm.math.reg.var.imp)
svm.math.reg.var.imp = svm.math.reg.var.imp[order(svm.math.reg.var.imp$importance, decreasing = T),]
xtable(svm.math.reg.var.imp)


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
svm.por.reg.error.train = mean((predict(svm.por.reg.fit) - por.train$grade.con)^2)

svm.por.reg.var.imp = numeric(length(predictors))
for(i in 1:length(predictors))
{
  svm.por.reg.fit.inf = do.call(svm, c(list(formula = as.formula(paste("grade.con~", paste(predictors[-i], collapse = "+"), sep = '')), data = por.train), svm.por.reg))
  
  svm.por.reg.var.imp[i] = mean((predict(svm.por.reg.fit.inf) - por.train$grade.con)^2) - svm.por.reg.error.train
}

svm.por.reg.var.imp = data.frame(predictors = predictors,
                                 importance = svm.por.reg.var.imp)
svm.por.reg.var.imp = svm.por.reg.var.imp[order(svm.por.reg.var.imp$importance, decreasing = T),]
xtable(svm.por.reg.var.imp[1:10,])

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
svm.por.cat.error.train = mean(predict(svm.por.cat.fit)!=por.train$grade.cat)
svm.por.cat.error.test

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
svm.math.cat.error.train = mean(predict(svm.math.cat.fit)!=math.train$grade.cat)
svm.math.cat.error.test

svm.math.cat.var.imp = numeric(length(predictors))
for(i in 1:length(predictors))
{
  svm.math.cat.fit.inf = do.call(svm, c(list(formula = as.formula(paste("grade.cat~", paste(predictors[-i], collapse = "+"), sep = '')), data = math.train), svm.math.cat))
  
  svm.math.cat.var.imp[i] = mean((predict(svm.math.cat.fit.inf) != math.train$grade.cat)) - svm.math.cat.error.train
}

svm.math.cat.var.imp = data.frame(predictors = predictors,
                                  importance = svm.math.cat.var.imp)
svm.math.cat.var.imp = svm.math.cat.var.imp[order(svm.math.cat.var.imp$importance, decreasing = T),]
xtable(svm.math.cat.var.imp[1:10,])

svm.por.cat.var.imp = numeric(length(predictors))
for(i in 1:length(predictors))
{
  svm.por.cat.fit.inf = do.call(svm, c(list(formula = as.formula(paste("grade.cat~", paste(predictors[-i], collapse = "+"), sep = '')), data = por.train), svm.por.cat))
  
  svm.por.cat.var.imp[i] = mean((predict(svm.por.cat.fit.inf) != por.train$grade.cat)) - svm.por.cat.error.train
}

svm.por.cat.var.imp = data.frame(predictors = predictors,
                                 importance = svm.por.cat.var.imp)
svm.por.cat.var.imp = svm.por.cat.var.imp[order(svm.por.cat.var.imp$importance, decreasing = T),]
xtable(svm.por.cat.var.imp[1:10,])

intersect(svm.por.cat.var.imp[1:10,1], svm.math.cat.var.imp[1:10,1])
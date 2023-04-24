library(readxl)
library(arsenal)
library(glmnet)
library(scales)
library(xtable)
library(kableExtra)
library(parallel)
library(foreach)
library(doParallel)

svm.cv.mine <- function(q3.train, form, k, response_variable)
{
  k = k
  folds = caret::createFolds(1:nrow(q3.train),k = k)

  gamma.seq = seq(from = 0.0001, to = 0.01, length.out = 50)
  cost = seq(from = 0.1, to = 5, length.out = 50)
  numeric.paras = expand.grid(1:50, 1:50)
  params.list = c(list(list(kernel = "linear")),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], degree = 2, cost = cost[numeric.paras[i,2]])}),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]], degree = 3)}),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]], degree = 4)}),
                  lapply(1:50^2, function(i){list(kernel = "radial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]])}),
                  lapply(1:50^2, function(i){list(kernel = "sigmoid", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]])})
  )
  
  numCores <- as.integer(future::availableCores()) - 1
  registerDoParallel(numCores)
  
  q3e.val.error = foreach(x=params.list) %dopar% {
    library(e1071)
    validation_error = numeric(k)
    for(i in 1:k){
      val.train = q3.train[-folds[[i]],]
      val.test = q3.train[folds[[i]],]
      
      fit = do.call(svm, c(list(formula = form, data = val.train), x))
      predict.test = predict(fit, val.test, decision.values = F)
      
      validation_error[i] = mean((predict.test - val.test[[response_variable]])^2)
    }
    return(mean(validation_error))
  }
  
  return(params.list[[which.min(q3e.val.error)]])
}

svm.cv.mine.cat <- function(q3.train, form, k, response_variable)
{
  k = k
  folds = caret::createFolds(1:nrow(q3.train),k = k)
  
  gamma.seq = seq(from = 0.0001, to = 0.01, length.out = 50)
  cost = seq(from = 0.1, to = 5, length.out = 50)
  numeric.paras = expand.grid(1:50, 1:50)
  params.list = c(list(list(kernel = "linear")),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], degree = 2, cost = cost[numeric.paras[i,2]])}),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]], degree = 3)}),
                  lapply(1:50^2, function(i){list(kernel = "polynomial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]], degree = 4)}),
                  lapply(1:50^2, function(i){list(kernel = "radial", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]])}),
                  lapply(1:50^2, function(i){list(kernel = "sigmoid", gamma = gamma.seq[numeric.paras[i,1]], cost = cost[numeric.paras[i,2]])})
  )
  
  numCores <- as.integer(future::availableCores()) - 1
  registerDoParallel(numCores)
  
  q3e.val.error = foreach(x=params.list) %dopar% {
    library(e1071)
    validation_error = numeric(k)
    for(i in 1:k){
      val.train = q3.train[-folds[[i]],]
      val.test = q3.train[folds[[i]],]
      
      fit = do.call(svm, c(list(formula = form, data = val.train), x))
      predict.test = predict(fit, val.test, decision.values = F)
      
      validation_error[i] = mean((predict.test != val.test[[response_variable]]))
    }
    return(mean(validation_error))
  }
  
  return(params.list[[which.min(q3e.val.error)]])
}

svm.cv.cost <- function(q3.train, form, k, response_variable, cost.list)
{
  
  n = length(cost.list)
  folds = caret::createFolds(1:nrow(q3.train),k = k)
  
  
  val.err.cost = numeric(n)
  for(j in 1:n)
  {
    validation_error = numeric(k)
    for(i in 1:k){
      val.train = q3.train[-folds[[i]],]
      val.test = q3.train[folds[[i]],]
      
      fit = svm(form, data = val.train, cost = cost.list[j], kernel = 'linear')
      predict.test = predict(fit, val.test, decision.values = F)
      
      validation_error[i] = mean(predict.test != val.test[[response_variable]])
    }
    val.err.cost[j] = mean(validation_error)
  }
  return(val.err.cost)
}

svm.cv.cost.reg <- function(q3.train, form, k, response_variable, cost.list)
{
  
  n = length(cost.list)
  folds = caret::createFolds(1:nrow(q3.train),k = k)
  
  
  val.err.cost = numeric(n)
  for(j in 1:n)
  {
    validation_error = numeric(k)
    for(i in 1:k){
      val.train = q3.train[-folds[[i]],]
      val.test = q3.train[folds[[i]],]
      
      fit = svm(form, data = val.train, cost = cost.list[j], kernel = 'linear')
      predict.test = predict(fit, val.test, decision.values = F)
      
      validation_error[i] = mean((predict.test - val.test[[response_variable]])^2)
    }
    val.err.cost[j] = mean(validation_error)
  }
  return(val.err.cost)
}

correlation_test <- function(x1, x2, type = "chisq")
{
  type1 = ifelse(length(unique(x1)) == 2, "binary", "continuous")
  type2 = ifelse(length(unique(x2)) == 2, "binary", "continuous")
  
  if(type1 == "binary" & type2 == "binary")
  {
    if(type == "chisq")
    {
      return(chisq.test(x1, x2, simulate.p.value = T)$p.value)
    }
    if(type == "fisher")
    {
      return(fisher.test(x1, x2)$p.value)
    }
  }
  if(type1 == "continuous" & type2 == "continuous")
  {
    return(summary(lm(x1~x2))$coefficients[2,4])
  }
  if(type1 == "categorical" & type2 == "continuous")
  {
    x3 = x1
    x1 = x2
    x2 = x3
  }
  x2 = as.factor(x2)
  x2 = c(0,1)[as.numeric(x2)]
  return(t.test(x1*x2, x1*(1-x2))$p.value)
}


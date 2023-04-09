library(readxl)
library(arsenal)
library(glmnet)
library(scales)
library(xtable)
library(kableExtra)
library(parallel)
library(foreach)
library(doParallel)

descriptive = function(sheet_name, data, descriptive_variables)
{
  my_controls <- tableby.control(
    total = T,
    numeric.stats = c("meansd",
                      "medianq1q3",
                      "range",
                      "Nmiss2"),
    
    cat.stats = c("countpct",
                  "Nmiss2"),
    
    stats.labels = list(
      meansd= "Mean (SD)",
      medianq1q3= "Median (Q1, Q3)",
      range= "Min - Max",
      Nmiss2= "Missing"
    )
  )
  descriptive_formula = as.formula(paste(response_variable, '~', paste(descriptive_variables, collapse = '+'),sep = ''))
  table1 <- tableby(descriptive_formula, 
                    data=data,
                    control = my_controls)
  tab1 = as.data.frame(summary(table1))
  tab1[,1] = gsub('&nbsp;', '', tab1[,1])
  # write.csv(tab1, file = paste(as.character(sheet_name), '_descriptive.csv', sep = ''))
  print(summary(table1, digits = 1))
}

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
      
      validation_error[i] = mean(predict.test != val.test[[response_variable]])
    }
    return(mean(validation_error))
  }
}

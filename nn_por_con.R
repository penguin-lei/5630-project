library(keras)
library(dplyr)


library(parallel)
library(foreach)
library(doParallel)
numCores <- as.integer(future::availableCores()) -2
registerDoParallel(numCores)

nn.model.por.reg = keras_model_sequential()


nunits = c(10, 5)
act_fun = c("relu", "elu")

nn.model.por.reg %>% layer_dense(units = nunits[1], activation = act_fun[1], input_shape = c(39)) %>%
  layer_dense(units = 1)

nn.model.por.reg %>% compile(loss = "mse",
                              optimizer = 'rmsprop',
                              metrics = 'mse')

nn.model.por.reg.fit <- nn.model.por.reg %>% fit(as.matrix(por.train[,predictors]), 
                                                   por.train$grade.con, 
                                                   epochs = 20, 
                                                   batch_size = 8,
                                                   validation_split = 0.2)
mean((nn.model.por.reg %>% predict(as.matrix(por.test[,predictors])) - por.test$grade.con)^2)

nn.model.por.reg %>% evaluate(as.matrix(por.test[,predictors]), por.test$grade.con)


paras_list = expand.grid(c(5, 10, 20), c(2, 4, 5, 10), 
                         c("relu", "elu", "selu", "sigmoid"),
                         c("relu", "elu", "selu", "sigmoid"),
                         c(8, 16, 32),
                         c(10, 20, 40, 60, 80))
# set up a validation set to select parameters
set.seed(1)
val_d.idx = sample(nrow(por.train), 100, replace = F)
val_d = por.train[val_d.idx,]
train_d = por.train[-val_d.idx,]

val_error = numeric(nrow(paras_list))


options(keras.view_metrics = FALSE)
val_error = foreach(i = 1:nrow(paras_list)) %dopar% {
  library(keras)
  library(dplyr)
  nn.model.por.reg = keras_model_sequential()
  nn.model.por.reg %>% layer_dense(units = paras_list[i,1], activation = paras_list[i,3], input_shape = c(39)) %>%
    layer_dense(units = paras_list[i,2], activation = paras_list[i,4]) %>% 
    layer_dense(units = 1)
  
  nn.model.por.reg %>% compile(loss = "mse",
                               optimizer = 'rmsprop',
                               metrics = 'mse')
  
  nn.model.por.reg.fit <- nn.model.por.reg %>% fit(as.matrix(train_d[,predictors]), 
                                                   train_d$grade.con, 
                                                   epochs = paras_list[i,6], 
                                                   batch_size = paras_list[i,5],)
  
  return(mean((nn.model.por.reg %>% predict(as.matrix(val_d[,predictors])) - val_d$grade.con)^2))
  
}

set.seed(1)
for(i in 1:nrow(paras_list)){
  nn.model.math.reg = keras_model_sequential()
  nn.model.math.reg %>% layer_dense(units = paras_list[i,1], activation = paras_list[i,3], input_shape = c(39)) %>%
    layer_dense(units = paras_list[i,2], activation = paras_list[i,4]) %>% 
    layer_dense(units = 1)
  
  nn.model.math.reg %>% compile(loss = "mse",
                                optimizer = 'rmsprop',
                                metrics = 'mse')
  
  nn.model.math.reg.fit <- nn.model.math.reg %>% fit(as.matrix(train_d[,predictors]), 
                                                     train_d$grade.con, 
                                                     epochs = paras_list[i,6], 
                                                     batch_size = paras_list[i,5],)
  
  val_error[i] = mean((nn.model.math.reg %>% predict(as.matrix(val_d[,predictors])) - val_d$grade.con)^2)
}


paras_list[which.min(val_error),]


paras_list[which.min(val_error),]
# Var1 Var2 Var3 Var4 Var5 Var6
# 179   10   10 selu  elu    8   10

idx = 179
nn.model.por.reg = keras_model_sequential()
nn.model.por.reg %>% layer_dense(units = paras_list[idx,1], activation = paras_list[idx,3], input_shape = c(39)) %>%
  layer_dense(units = paras_list[idx,2], activation = paras_list[idx,4]) %>% 
  layer_dense(units = 1)

nn.model.por.reg %>% compile(loss = "mse",
                              optimizer = 'rmsprop',
                              metrics = 'mse')

nn.model.por.reg.fit <- nn.model.por.reg %>% fit(as.matrix(por.train[,predictors]), 
                                                   por.train$grade.con, 
                                                   epochs = paras_list[idx,6], 
                                                   batch_size = paras_list[idx,5],
                                                   validation_split = 0.2)

mean((nn.model.por.reg %>% predict(as.matrix(por.test[,predictors])) - por.test$grade.con)^2)
library(keras)
library(dplyr)



# neural network regression ----------------------
nn.model.math.reg = keras_model_sequential()


nunits = c(10, 5)
act_fun = c("relu", "elu")

nn.model.math.reg %>% layer_dense(units = nunits[1], activation = act_fun[1], input_shape = c(39)) %>%
  layer_dense(units = 1)

nn.model.math.reg %>% compile(loss = "mse",
                  optimizer = 'rmsprop',
                  metrics = 'mse')

nn.model.math.reg.fit <- nn.model.math.reg %>% fit(as.matrix(math.train[,predictors]), 
                                                   math.train$grade.con, 
                                                   epochs = 20, 
                                                   batch_size = 8,
                                                   validation_split = 0.2)
mean((nn.model.math.reg %>% predict(as.matrix(math.test[,predictors])) - math.test$grade.con)^2)

nn.model.math.reg %>% evaluate(as.matrix(math.test[,predictors]), math.test$grade.con)


paras_list = expand.grid(c(5, 10, 20), c(2, 4, 5, 10), 
                         c("relu", "elu", "selu"),
                         c("relu", "elu", "selu"),
                         c(4, 8, 16, 32),
                         c(10, 20, 40, 60, 80))
# set up a validation set to select parameters
val_d.idx = sample(nrow(math.train), 100, replace = F)
val_d = math.train[val_d.idx,]
train_d = math.train[-val_d.idx,]

val_error = numeric(nrow(paras_list))

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

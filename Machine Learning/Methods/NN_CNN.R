setwd("/Users/kieranschubert/Desktop/Statistics/2nd_Semester/Machine_Learning/TPs/TP5/Data")
install.packages("tensorflow")
install.packages("keras")
library(tensorflow)
library(keras)
install_tensorflow(version = "1.12")

## EX2 ##

ziptrain <- read.csv(file='zip.train',sep='')
ziptest <- read.csv(file='zip.test',sep='') 
colnames(ziptrain) <- c('Y',paste('X',rep(1:256),sep='')) 
colnames(ziptest) <- c('Y',paste('X',rep(1:256),sep=''))

ziptrainPred = as.matrix(ziptrain)[,-1] 
ziptestPred = as.matrix(ziptest)[,-1]

# One hot encode training target values
ziptrainLabels <- to_categorical(ziptrain$Y)

# One hot encode test target values
ziptestLabels <- to_categorical(ziptest$Y)

# Initialize a sequential model
model <- keras_model_sequential()

# Add layers to the model
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(256)) %>% 
  layer_dense(units = 10, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy', optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(ziptrainPred, ziptrainLabels,
                          epochs = 4,
                          batch_size = 5,
                          validation_split = 0.2
)
plot(history)

#Test error
testPredictions <- model %>% predict_classes(ziptestPred) 
1-mean(testPredictions == ziptest[,1])

#Train error
trainPredictions <- model %>% predict_classes(ziptrainPred) 
1-mean(trainPredictions == ziptrain[,1])


model <- keras_model_sequential()
# Add layers to the model
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(256)) %>% layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy', optimizer = 'adam',
  metrics = 'accuracy'
)
history <- model %>% fit( ziptrainPred, ziptrainLabels,
                          epochs = 8,
                          batch_size = 5,
                          validation_split = 0.2, 
                          callbacks = list(
                            callback_early_stopping(monitor = "val_loss", 
                            restore_best_weights = TRUE)) )
plot(history)

## EX3 ##
ziptrainPred2d = array_reshape(ziptrainPred, c(nrow(ziptrainPred), 16,16,1))
ziptestPred2d = array_reshape(ziptestPred, c(nrow(ziptestPred), 16,16,1))

model <- keras_model_sequential()
# Add layers to the model
model %>%
  layer_conv_2d(filters = 16, kernel_size = 5, padding = "same", input_shape = c(16,16,1)) %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_conv_2d(filters = 16, kernel_size = 2, padding = "same") %>% layer_max_pooling_2d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy', optimizer = 'adam',
  metrics = 'accuracy'
)
history <- model %>% fit( ziptrainPred2d, ziptrainLabels,
                          epochs = 8,
                          batch_size = 5, validation_split = 0.2, callbacks = list(
                            callback_early_stopping(monitor = "val_loss", restore_best_weights = TRUE)) )
plot(history)

# Test error
testPredictions <- model %>% predict_classes(ziptestPred2d) 
1-mean(testPredictions == ziptest[,1])

# Train error
trainPredictions <- model %>% predict_classes(ziptrainPred2d)
1-mean(trainPredictions == ziptrain[,1])



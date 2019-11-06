MSE = 0
folds = cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)

for (i in 1 : 10) {
  testIndices = which(folds == i, arr.ind = TRUE)
  testData = data[testIndices,]
  trainingData = data[-testIndices,]
  
  trModel = lm(y ~ x1 + x2 + x3 + x4 + x5 + x7, data = trainingData, x = T, y = T)
  predicted = predict(trModel, testData)
  error = abs(predicted - testData$y)
  squaredError = error * error
  MSE[i] = mean(squaredError)
}

plot(MSE)
lines(x = MSE, y = NULL, type = 'l', col = 'blue')

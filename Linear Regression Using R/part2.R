trsumS = 0
tssumS = 0
trMSE = 0
tsMSE = 0
p = 0

for (i in 1 : 10) {
  trSize = 10 * i
  trainingData = head(data, trSize)
  trModel = lm(y ~ x1 + x2 + x3 + x4 + x5 + x7, data = trainingData, x = T, y = T)
  predicted = predict(trModel, trainingData)
  trError = abs(predicted - trainingData$y)
  trSquaredError = trError * trError
  trsumS[i] = sum(trSquaredError)
  trMSE[i] = mean(trSquaredError)
  
  testData = head(data, -trSize)
  predicted = predict(trModel, testData)
  tsError = abs(predicted - testData$y)
  tsSquaredError = tsError * tsError
  tssumS[i] = sum(tsSquaredError)
  tsMSE[i] = mean(tsSquaredError)
}

plot(trsumS, main = 'effect of size of training and test error')
lines(x = trsumS, y = NULL, type = 'l', col = 'blue')
points(tssumS, pch = 10, col = 'red')
lines(x = tssumS, y = NULL, type = 'l', col = 'red')

plot(trMSE, main = 'effect of size on training MSE(blue) and test MSE(red)')
lines(x = trMSE, y = NULL, type = 'l', col = 'blue')
points(tsMSE, pch = 10, col = 'red')
lines(x = tsMSE, y = NULL, type = 'l', col = 'red')


###part 1


air= airquality
head(air)

sumS=0

summary(air)

#ozone and solar.r have null values

air$Ozone[is.na(air$Ozone)]=mean(air$Ozone,na.rm=T)
air$Solar.R[is.na(air$Solar.R)]=mean(air$Solar.R,na.rm=T)
summary(air)

##
model=lm(air$Ozone~air$Solar.R)
print(model)
plot(air$Solar.R,air$Ozone)
abline(model,col="red")

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[1]=sum(squaredError)

##
model=lm(air$Ozone~air$Wind)
print(model)
plot(air$Wind,air$Ozone)
abline(model,col="red")

predictions = predict(model,air)
plot(predictions,xlab = "ozone as wind ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[2]=sum(squaredError)

##
model=lm(air$Ozone~air$Temp)
print(model)
plot(air$Temp,air$Ozone)
abline(model,col="red")

predictions = predict(model,air)
plot(predictions,xlab = "ozone as temp ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[3]=sum(squaredError)

##
model=lm(air$Ozone~air$Month)
print(model)
plot(air$Month,air$Ozone)
abline(model,col="red")

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[4]=sum(squaredError)

##
model=lm(air$Ozone~air$Day)
print(model)
plot(air$Day,air$Ozone)
abline(model,col="red")

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[5]=sum(squaredError)

##
model=lm(air$Ozone~air$Solar.R+air$Wind)
print(model)

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[6]=sum(squaredError)


##
model=lm(air$Ozone~air$Solar.R+air$Wind+air$Temp)
print(model)

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[7]=sum(squaredError)

##
model=lm(air$Ozone~air$Solar.R+air$Wind+air$Temp+air$Month)
print(model)

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[8]=sum(squaredError)

##
model=lm(air$Ozone~air$Solar.R+air$Wind+air$Temp+air$Month+air$Day)
print(model)

predictions = predict(model,air)
plot(predictions,xlab = "ozone as solar.r ")
points(air$Ozone,col="red")

error=abs(predictions-air$Ozone)
squaredError=error*error
sumS[9]=sum(squaredError)

####
plot(sumS, type = 'l')




#############part 2 

trsumS=0
tssumS=0
trMSE=0
tsMSE=0

for( i in 1:10 ){
  
  trsize=10*i
  trainingdata=head(air,trsize)
  model=lm(trainingdata$Ozone~trainingdata$Solar.R+trainingdata$Wind+trainingdata$Temp+trainingdata$Month+trainingdata$Day)
  predicted = predict(model,trainingdata)
  error=abs(predicted-trainingdata$Ozone)
  squaredError=error*error
  trsumS[i]=sum(squaredError)
  trMSE[i]=mean(squaredError)
  
  testdata=head(air,-trsize)
  predicted = predict(model,testdata)
  error=abs(predicted-testdata$Ozone)
  squaredError=error*error
  tssumS[i]=sum(squaredError)
  tsMSE[i]=mean(squaredError)
  
}

plot(trMSE, main = 'effect of size on training MSE(blue) and test MSE(red)')
lines(x = trMSE, y = NULL, type = 'l', col = 'blue')
plot(tsMSE, pch = 10, col = 'red')
lines(x = tsMSE, y = NULL, type = 'l', col = 'red')


plot(trsumS, main = 'effect of size of training and test error')
lines(x = trsumS, y = NULL, type = 'l', col = 'blue')
plot(tssumS, pch = 10, col = 'red')
lines(x = tssumS, y = NULL, type = 'l', col = 'red')



###part 3
MSE = 0
folds = cut(seq(1, nrow(air)), breaks = 10, labels = FALSE)

for (i in 1 : 10) {
  testIndices = which(folds == i, arr.ind = TRUE)
  testdata = air[testIndices,]
  trainingdata = air[-testIndices,]
  
  model=lm(trainingdata$Ozone~trainingdata$Solar.R+trainingdata$Wind+trainingdata$Temp+trainingdata$Month+trainingdata$Day)
  predicted = predict(model,testdata)
  error=abs(predicted-testdata$Ozone)
  squaredError=error*error
  MSE[i] = mean(squaredError)
}

plot(MSE)
lines(x = MSE, y = NULL, type = 'l', col = 'blue')

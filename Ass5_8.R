setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment5/")
data1 = read.table("ExSheet8Data.txt", header = TRUE)

x = data1$x
y = data1$y
library(fields)

#a)

find_press = function(x, y, lambda){
  n = length(y)
  pred = vector(length=n)
  
  for(i in 1:n){
    modeli = sreg(x[-i], y[-i], lambda=lambda)
    pred[i] = predict(modeli, x[i])
  }
  
  cv_res = y - pred
  press = sum(cv_res^2)
  return(press)
}

lambda = seq(from=5, to=15, by=0.5)
press = vector(length=length(lambda))
for(j in 1:length(lambda)){
  press[j] = find_press(x, y, lambda[j])
}
press==min(press)
lambda[press==min(press)] # This is optimal lambda
find_press(x,y,lambda[press==min(press)]) # press stat for optimal lambda


#b)
modelgcv = sreg(x, y)
h = modelgcv$lambda # lambda for sreg
modelgcv$eff.df

find_press(x,y,h) # press for sreg
#c)
model0 = sreg(x, y, lambda=lambda[press==min(press)])
plot(x=x, y=y, col="blue")
lines(model0$predicted, col="red") #optimal lambda modelcurve drawn in red.
lines(modelgcv$predicted, col="purple")


setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment5/")
data = read.table("ExSheet9Data.txt", header = TRUE)

n=nrow(data)

model1 = glm(success ~ sex, family=binomial, data=data)

ll_cv1 = vector(length = n)
for (i in 1:n){
  modeli = glm(success ~ sex, family=binomial, data = data[-i,])
  pi = predict(modeli, newdata = data[i,], type = 'response')
  ll_cv1[i] = data$success[i]*log(pi) + (1-data$success[i])*log(1-pi)
}
sum(ll_cv1)

model2 = glm(success ~ age, family=binomial, data=data)
model2

ll_cv2 = vector(length = n)
for (i in 1:n){
  modeli = glm(success ~ age, family=binomial, data = data[-i,])
  pi = predict(modeli, newdata = data[i,], type = 'response')
  ll_cv2[i] = data$success[i]*log(pi) + (1-data$success[i])*log(1-pi)
}
sum(ll_cv2)

model3 = glm(success ~ smoke, family=binomial, data=data)
model3

ll_cv3 = vector(length = n)
for (i in 1:n){
  modeli = glm(success ~ smoke, family=binomial, data = data[-i,])
  pi = predict(modeli, newdata = data[i,], type = 'response')
  ll_cv3[i] = data$success[i]*log(pi) + (1-data$success[i])*log(1-pi)
}
sum(ll_cv3)

#b)
model4 = glm(success ~ age + proc, family=binomial, data = data)

ll_cv4 = vector(length = n)
for (i in 1:n){
  modeli = glm(success ~ age + proc, family=binomial, data = data[-i,])
  pi = predict(modeli, newdata = data[i,], type = 'response')
  ll_cv4[i] = data$success[i]*log(pi) + (1-data$success[i])*log(1-pi)
}
sum(ll_cv4)

model4$coefficients[3] #coefficient for proc

#c)
table(data$proc) #proc 0 has more occurences 
mean(data$age) # mean age = 44.04

f = function(v){
  return(c(mean(v), length(v)))
}

T = aggregate(success~age+proc, FUN=f, data=data) 
#observing T, it can be seen that there are no success=2 with procedure 1
No_of_successes = T$success[T$success==2][2]
sizeT = nrow(T)
prob = (No_of_successes/sizeT)*100 #probability of success for mean age and proc 0.


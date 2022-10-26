setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 4")
Dataset = read.table("Ex7DataSet.txt", header = TRUE)
head(Dataset)
n = nrow(Dataset)

# FIRST MODEL
Model1 = lm(y ~ x1, data = Dataset)

pred1 = vector(length=n)
for(i in 1:n){
  modeli = lm(y ~ x1, Dataset[-i,])
  pred1[i] = predict(modeli, Dataset[i,])
}
cv_res1 = Dataset$y - pred1
PRESS1 = sum(cv_res1^2)

# SECOND MODEL
Model2 = lm(y ~ x2, data = Dataset)

pred2 = vector(length=n)
for(i in 1:n){
  modeli = lm(y ~ x2, Dataset[-i,])
  pred2[i] = predict(modeli, Dataset[i,])
}
cv_res2 = Dataset$y - pred2
PRESS2 = sum(cv_res2^2)


# THIRD MODEL
Model3 = lm(y ~ x3, data = Dataset)

pred3 = vector(length=n)
for(i in 1:n){
  modeli = lm(y ~ x3, Dataset[-i,])
  pred3[i] = predict(modeli, Dataset[i,])
}
cv_res3 = Dataset$y - pred3
PRESS3 = sum(cv_res3^2)

# b) 

Model4 = lm(y ~ x1 + x4, data = Dataset)

pred4 = vector(length=n)
for(i in 1:n){
  modeli = lm(y ~ x1 + x4, Dataset[-i,])
  pred4[i] = predict(modeli, Dataset[i,])
}
cv_res4 = Dataset$y - pred4
PRESS4 = sum(cv_res4^2)


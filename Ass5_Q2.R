#///Q2a)\\\
library(bootstrap)
setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 1")
data2 = read.table("exercise1_170372134.txt", header = TRUE)
head(data2)

#A and B are vectors containing the sample data control and treatment.
A = data2$control
B = data2$treatment

#using bootstrap percentile method to calculate standard error and a 95% confidence interval for diff of mean
N=1000
bootreps = vector(length=N, mode="numeric")
for(i in 1:N){
  Ab = sample(A, replace=TRUE)
  Bb = sample(B, replace=TRUE)
  bootreps[i] = mean(Ab) - mean(Bb)
}

se = sd(bootreps) #standard error

alpha = (1-95/100)/2
k = floor(alpha*(N+1)) #k-value corresponding to 95% confidence interval
sb=sort(bootreps) #sorting bootreps in order 

ThetaL = sb[k] #lower boundary for 95% CI
ThetaU = sb[N+1-k] #upper boundary

#///Q2b)\\\

#r = seC/seT
#using bootstrap percentile method to calculate a 95% confidence interval for variability(r)
N=1000
bootreps = vector(length=N, mode="numeric")
for(i in 1:N){
  Ab = sample(A, replace=TRUE)
  Bb = sample(B, replace=TRUE)
  bootreps[i] = sd(Ab)/sd(Bb)
}
alpha = (1-95/100)/2
k = floor(alpha*(N+1))
sb=sort(bootreps)

ThetaL = sb[k]
ThetaU = sb[N+1-k] #confidence limits for r


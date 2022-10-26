setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 2")
data1 = read.table("exercise3_170372134.txt", header = TRUE)
head(data1)

#///Q1a)///
x = data1$before #before marks
y = data1$after #after marks
z = x-y  #vector of differences between before and after 

#using bootstrap percentile method to calculate a 95% confidence interval for mean of differences
N=2000 #number of bootstrap sample repetitions
bootreps = vector(length=N, mode="numeric")
for(i in 1:N){
  zb = sample(z, replace=TRUE)
  bootreps[i] = mean(zb)
}

alpha = (1-95/100)/2 
k = floor(alpha*(N+1)) #k-value corresponding to 95% confidence interval
sb=sort(bootreps)

ThetaL = sb[k]
ThetaU = sb[N+1-k] #95% percentile confidence level between these 2 values.

#///Q1b)///
library(bootstrap)

#functions defined in order to run BCa method.
theta_hat = function(z){
  return(mean(z))
}

#Using BCa bootstrap method to calculate a 95% confidence interval for mean of differences
bca_results1 = bcanon(z, theta=theta_hat, nboot=N, alpha=c(0.025, 0.975))
A = bca_results1$confpoints
LowerTheta = A[1,2] 
UpperTheta = A[2,2]
# This confidence interval tells us that 95% of the data is between LowerTheta and UpperTheta


#Q2a)
setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 1")
data2 = read.table("exercise1_170372134.txt", header = TRUE)
head(data2)

#A and B are vectors containing the sample data control and treatment.
A = data2$control
B = data2$treatment

#using bootstrap percentile method to calculate standard error and a 95% confidence interval
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




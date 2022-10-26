library(perm)
setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 1")
data = read.table("exercise1_170372134.txt", header = TRUE)
head(data)

#A and B are vectors containing the sample data control and treatment.
A = data$control
B = data$treatment

#Monte Carlo method with 20,000 replications:
permTS(x=A, y=B, method="exact.mc", control = permControl(nmc=20000, setSEED=FALSE, tsmethod="abs"))
permTS(x=A, y=B, method="exact.mc", control = permControl(nmc=20000, setSEED=FALSE, tsmethod="central"))

n = length(A)
m = length(B)
#number of all possible combinations for the permutation distribution
choose(m+n,m)

#Compute two-sided t-test.
t.test(x=A, y=B, alternative="two.sided")
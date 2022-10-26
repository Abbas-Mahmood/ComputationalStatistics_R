setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 2")
data = read.table("exercise3_170372134.txt", header = TRUE)
head(data)

x = data$before
y = data$after
z = x-y  #vector of differences between before and after 

w = sum(rank(abs(z))*(z>0)) #sum of all positive ranks (W+)

#Mann Whitney signed rank test using normal approx
wilcox.test(x=z, exact=FALSE, correct=FALSE) 
wilcox.test(x=z, exact=FALSE) # with continuity correction

#one sided tests
wilcox.test(x=z, exact=FALSE, correct=FALSE,alternative = "greater") 
wilcox.test(x=z, exact=FALSE, correct=FALSE,alternative = "less") 



setwd("/Users/abbasmahmood/Desktop/Maths QM/MSC/Computational Stats with R (MTH791P)/Assignment 2")
TestScores = read.table("exercise3_170372134.txt", header = TRUE)
#x = TestScores$before
#y = TestScores$after

#Fitting linear model to the original dataset
Model1 = lm(after ~ before, data = TestScores)
Model1
summary(Model1) #Summary - including standard error for the coefficient 
Model1$coefficients[2] #Coefficient for the slope


library(bootstrap)

n = nrow(TestScores)
N = 10000 #Number of bootstrap replications

#Bootstrap cases
bootreps = vector(length = N, mode = 'numeric')
for (i in 1:N){
  index = sample(1:n, replace = TRUE) 
  TestScores_boot = TestScores[index,]
  Model_i = lm(after ~ before, data = TestScores_boot)
  bootreps[i] = Model_i$coefficients[2]
}

#Bootstrap estimate for standard error of the slope in the linear model
sd(bootreps) 

#95% percentile CI
quantile(bootreps, probs = c(0.025,0.975))

#Bootstrap Residuals

fitted = Model1$fitted.values
resid = Model1$residuals

bootreps_resid = vector(length = N, mode = 'numeric')
for (i in 1:N){
  resid_boot = sample(resid, replace = TRUE)
  ystar = fitted + resid_boot
  Model_i = lm(ystar ~ TestScores$before)
  bootreps_resid[i] = Model_i$coefficients[2]
}
#Bootstrap estimate for standard error of the slope in the linear model
sd(bootreps_resid) 

#95% percentile CI
quantile(bootreps_resid, probs = c(0.025,0.975))


#BCA bootstrap

theta_hat = function(units,xy){
  
  LinearModel = lm(after[units] ~ before[units], data = xy)
  return(LinearModel$coefficients[2])
}

lm_bca = bcanon(1:n, theta_hat, nboot=10000, TestScores, alpha = c(0.025,0.975))
#95% percentile CI
A = lm_bca$confpoints
Theta_L = A[1,2] 
Theta_U = A[2,2]







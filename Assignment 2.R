C = c(65.6,68.1,59.3,61.1,61.5,61.2) 
T = c(52.9,56.8,72.0,49.7,63.6,53.1)
m = length(C)
n = length(T)

CT = c(C,T) 
r = rank(CT)

#observed test stat values
w0 = sum(r[1:m]) 
u0 = w0 - m*(m+1)/2

# Computation for pU = P(Ux >= u0)
pU = 1-pwilcox(u0-1,m=m,n=n) 
pU

#Mean and Variance for Ux
muU = m*n/2
varU = m*n*(m*n+1)/12
sdU = sqrt(varU)

#Normal apporximation 
z=(u0-muU)/sdU
1-pnorm(z) 


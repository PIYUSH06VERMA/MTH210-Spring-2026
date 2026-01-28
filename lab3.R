##2
f2<-function(x)
{
  return(exp(x+x^2))
}

N<-100000
x<-runif(N,-2,2)
I<-4*mean(f2(x))
cat("The integral value is ", I)
##3
f3<-function(theta)
{
  return(sin(theta)*cos(theta))
}
theta<-runif(N,0,pi/2)
I3<-(pi/2)*mean(f3(theta))
cat("The value of Integral is ",I3)
##4
x<-runif(N,0,1)
y<-runif(N,0,1)
f4<-function(x,y)
{
  res<-exp((x+y)^2)
  return(res)
}
I4<-mean(f4(x,y))
cat("The vale of integral is ", I4)

##5

N<-1000
obs<-numeric(length=N)
exp_gen<-function()
{
  u<-runif(1)
  x<-(-log(1-u))
  return(x)
}
for(i in 1:N)
{
  X<-replicate(11,exp_gen())
  obs[i]<-sort(X)[6]
}
EX<-mean(obs)
varX<-var(obs)
cat("The mean and variance of X(6) is ", EX,varX)
##1
generate<-function(n){
  smpl<-runif(n,min=0,max=1)
  return(smpl)
}
##2
plot_gen<-function(n){
  data_pts<-generate(n)
  plt<-hist(data_pts)
  print(plt)
}
##3
div<-function(k){
  seg<-0:k
  seg<-seg/k
  return(seg)
}
## or i can do it using seq(0,1,1/k)
##4
computation<-function(n,k)
{
  temp<-generate(n)
  temp<-floor(temp*k)+1
  obs<-numeric(length=k)
  for(i in 1:n)
  {
    obs[temp[i]]=obs[temp[i]]+1
  }
  y<-(n/k-obs)^2
  y<-y/(n/k) ## mistake in assn Oi and Ei symbols exchanged
  chi2<-sum(y)
  return(chi2)
}
computation(50,5)
##5
N<-c(50,50,50,100,100,100,200,200,200,1000,1000,1000)
K<-c(5,10,15,5,10,15,5,10,15,5,10,15)
res<-numeric(length=12)
for(i in 1:12)
{
  res[i]<-computation(N[i],K[i])
}
##6
final_table<-data.frame(
  N=N,
  K=K,
  chi2=res
)

final_table

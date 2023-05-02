#ex1.1
density_exp=function(lambda,n,a){
  x=seq(0,a,n);
  y=dexp(x,lambda);
  plot(x,y,type="l");
}
density_exp(2,0.1,10);
#1.1 a
density_gamma=function(alpha,lambda,n,a){
  x=seq(0,a,n);
  y=dgamma(x,alpha,rate=lambda);
  plot(x,y,type="l");
}
density_gamma(5,1/2,0.1,10);
#1.1 b
density_student=function(r,n,a){
  x=seq(-a,a,n);
  y=dt(x,r);
  plot(x,y,type="l");
}
density_student(10,0.1,5);
#1.1 c
density_normal=function(u,s){
  x=seq(u-4*sqrt(s),u+4*sqrt(s),100);
  y=dnorm(x,mean=u,sd=sqrt(s),log=FALSE);
  plot(x,y,type="l");
}

#2.1 
#a
LLN_Exp = function(lambda, n) {
  return(mean(rexp(n,lambda)));
}
#b
LLN_Bin = function(m,p,n) {
  return(mean(rbinom(n,m,p)));
}
LLN_Bin(5,0.2,10000000);
#2.2
LLN_Student = function(r,n) {
  return(mean(rt(n,r)));
}
n=c(1000, 10000, 100000, 1000000);
r=c(2, 3, 4, 5);
for(i in n)
  for(j in r)
    {res=LLN_Student(j,i);
    cat("results:",res,'\n');
    }
#3.1
CLT_Exp = function(lambda, n, N, z) {
  expectation = 1/lambda;
  st_dev=1/lambda;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n=mean(rexp(n, lambda));
    if(x_n<=upper_bound) {
      sum=sum + 1;
    }
  }
  return(c(sum/N,pnorm(z)));
}
CLT_Exp(2,1000,100000,1);
#3.2
CLT_Gamma = function(alpha, lambda, n, N, z) {
  expectation = alpha/lambda;
  st_dev = sqrt(alpha)/lambda;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rgamma(n, alpha, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(c(sum/N, pnorm(z)));
}
N=c(5000, 10000, 20000);
z=c(-1.5, 0, 1.5);
for (i in N)
  for (j in z)
  {res=CLT_Gamma(1,2,50,i,j);
    cat(res,'\n');
    }
#4.1
binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k + 0.5)/standard_deviation;
  return(pnorm(q));
}
binomial_probability(50,0.3,10);
#4.2
binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k + 0.5-expectation)/standard_deviation;
  return(1 - pnorm(q));
}
binomial_probability(50,0.3,10);

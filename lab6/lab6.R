#1.1
selection_mean = function(filename) {
  x = scan(filename);
  m = mean(x)
  return(m)
}
selection_mean("history.txt")
#2
alfa=0.1
sample_mean=20
n=100
sigma=sqrt(9)
critical_z=qnorm(1-alfa/2,0,1)
a=sample_mean-critical_z*sigma/sqrt(n)
b=sample_mean+critical_z*sigma/sqrt(n)
interval=c(a,b)
interval
#2.1
zconfidence_interval=function(n,sample_mean,alfa,sigma){
  critical_z=qnorm(1-alfa/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  return(interval)
}
#2.2
zconfidence_interval(25,67.53,0.1,10)
#2.3
zconfidence_interval(50,5,0.05,0.5)
#2.6
zconfidence_interval_file <- function(file_path, alfa) {
  data <- scan(file_path)
  
  sample_mean <- mean(data)  
  n <- length(data)  
  sigma <- sd(data)
  critical_z <- qnorm(1 - alfa / 2, 0, 1)
  a <- sample_mean - critical_z * sigma / sqrt(n)
  b <- sample_mean + critical_z * sigma / sqrt(n)
  
  interval <- c(a, b)
  return(interval)
}
file_path <- "history.txt" 
alfa <- 0.05  # nivelul de semnificație (1 - nivelul de încredere)

interval <- zconfidence_interval_file(file_path, alfa)
interval

#3
alfa=0.05
sample_mean=3.3
n=60
s=0.4
se=s/sqrt(n)
critical_t=qt(1-alfa/2,n-1)
a=sample_mean-critical_t*se
b=sample_mean+critical_t*se
interval=c(a,b)
interval
#3.1
t_conf_interval=function(n,sample_mean,s,alfa){
  se=s/sqrt(n)
  critical_t=qt(1-alfa/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  return(interval)
}
#3.2
t_conf_interval(196,44.65,sqrt(2.25),0.01)
#3.3
#a
t_conf_interval(49,12,1.75,0.01)
t_conf_interval(49,12,1.75,0.05)
#b
t_conf_interval(49,13.5,1.25,0.07)
#3.4
t_conf_interval_file <- function(file_path, alfa) {
  data <- scan(file_path) 
  n <- length(data)  
  sample_mean <- mean(data) 
  s <- sd(data)  
  
  se <- s / sqrt(n)
  critical_t <- qt(1 - alfa / 2, n - 1)
  a <- sample_mean - critical_t * se
  b <- sample_mean + critical_t * se
  
  interval <- c(a, b)
  return(interval)
}
file_path <- "history.txt" 
alfa <- 0.05  # nivelul de semnificație (1 - nivelul de încredere)

interval <- t_conf_interval_file(file_path, alfa)
interval
#3.5
file_path <- "sample.txt" 
alfa <- 0.1  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 90%
interval_90 <- t_conf_interval_file(file_path, alfa)
interval_90
alfa <- 0.05  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 95%
interval_95 <- t_conf_interval_file(file_path, alfa)
interval_95
alfa <- 0.01  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 99%
interval_99 <- t_conf_interval_file(file_path, alfa)
interval_99

#4
alfa=0.01
n=100
succese=63
p_prim=succese/n
p0=0.6
z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
critical_z=qnorm(1-alfa,0,1)
z_score
critical_z
#4.1
test_proportion = function(alfa,n,succese,p0,tip_ip){
  p_prim = succese/n
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  #cat("z_score=",z_score)
  if(tip_ip=="r"){
    critical_z = qnorm(1 - alfa, 0, 1)
   cat("critical_z=",critical_z)
    if(z_score<=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip=="l"){
    critical_z = qnorm(alfa, 0, 1)
   cat("critical_z=",critical_z)
    if(z_score>=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip=="s"){
    critical_z = qnorm(1-alfa/2, 0, 1)
   cat("critical_z=",critical_z)
    if(abs(z_score)<=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
}
#4.2
test_proportion(alfa = 0.05, n = 150, succese = 20, p0 = 0.10, tip_ip = "r")

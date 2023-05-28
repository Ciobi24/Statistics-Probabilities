#B1
LLN_Geometric <- function(n, p) {
  return(c(mean(rgeom(n,p)),1/p));
}
LLN_Geometric(5000, 0.2)
LLN_Geometric(10000, 0.6)
LLN_Geometric(100000, 0.6)
LLN_Geometric(500000, 0.8)

#B2
CLT_Student <- function(r, n, N, z) {
  expectation <- 0
  st_dev <- sqrt(r / (r - 2))
  upper_bound <- z * st_dev / sqrt(n) + expectation
  sum <- 0
  
  for (i in 1:N) {
    x_n <- mean(rt(n, r))
    if (x_n <= upper_bound) {
      sum <- sum + 1
    }
  }
  
  return(sum/N)
}
n=50
r=3
val=CLT_Student(r,n,5000,-1.5)
theoretical_prob <- pnorm(-1.5)
print(abs(val-theoretical_prob))
val=CLT_Student(r,n,10000,0)
theoretical_prob <- pnorm(0)
print(abs(val-theoretical_prob))
val=CLT_Student(r,n,20000,1.5)
theoretical_prob <- pnorm(1.5)
print(abs(val-theoretical_prob))

#B3
MoivreLaplaceBinomial <- function(n, p, h, k) {
  expectation <- n * p
  variance <- n * p * (1 - p)
  standard_deviation <- sqrt(variance)
  q1 <- (h + 0.5 - expectation) / standard_deviation
  q2 <- (k + 0.5 - expectation) / standard_deviation
  approx_prob <- pnorm(q2) - pnorm(q1)
  return(approx_prob)
}


disc_area=function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return(4*N_C/N); }

#1.1
sphere_Vol = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z=runif(1, -1, 1);
    if(x*x + y*y+z*z <= 1)
      N_C = N_C + 1;
  }
  return(8*N_C/N); }
abs_err=abs(sphere_Vol(10000)-4*pi/3);
rel_err=abs_err/(4*pi/3)*100;
print(abs_err);
print(rel_err);

#1.2
parabola <- function(x) {
  return(-2*x^2 + 5*x - 2);
}
n <- 10000;
x <- runif(n, 0, 2);
y <- parabola(x);
aria_estimata <- 2 * 2 * sum(y >= 0) / n;
aria_exacta <- integrate(parabola, 0, 2)$value;
eroare_relativa <- abs(aria_exacta - aria_estimata) / aria_exacta;
cat("Aria estimata: ", aria_estimata, "\n");
cat("Aria exacta: ", aria_exacta, "\n");
cat("Eroarea relativa: ", eroare_relativa, "\n");

#2
MC_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  return(10*sum/N);
}
MC_integr_average= function(k, N) {
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}
MC_improved_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    sum = sum + exp(-u*u)/exp(-u);
  }
  return(sum/N);
}
MC_imprvd_integr_average= function(k, N) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}
#2.1
MC_integration_sin = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, pi);
    sum = sum + sin(u)^2;
  }
  return(pi*sum/N);
}

MC_integration_rad = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, 1);
    sum = sum + 1/sqrt(1-u^2);
  }
  return(sum/N);
}

#2.1 a)
MCintegrationa = function(N) {  #nr de puncte generate pentru estimarea integralei
  sum = 0; #suma valorilor fct in punctele generate
  for(i in 1:N) {
    u = runif(1, 0, pi); #Ui luam variabilele uniforme
    sum = sum + sin(u)^2; # suma din h(Ui)
  }
  return(pi*sum/N); #(b-a)/N *suma de la 1 la N de h(Ui)
}
intMC=MCintegrationa(10000)
print(intMC)

print(cat("eroarea abs este ", abs(intMC-pi/2)))

print(cat("eroarea relativa", abs(intMC-pi/2)/(pi/2)))



#2.1 b si d
MC_integration <- function(a, b, f, N) {
  sum <- 0;
  for (i in 1:N) {
    x <- runif(1, a, b);
    sum <- sum + f(x);
  }
  return(sum * (b - a) / N);
}

# (b) Estimarea integralei Z 4 1 e^x dx
f_b <- function(x) {
  return(exp(x));
}
a_b <- 1;
b_b <- 4;
exact_value_b <- 51.87987;

N_b <- 50000;
estimated_value_b <- MC_integration(a_b, b_b, f_b, N_b);

absolute_error_b <- abs(estimated_value_b - exact_value_b);
relative_error_b <- abs((estimated_value_b - exact_value_b) / exact_value_b) * 100;

cat("Valoarea estimată pentru integrala Z 4 1 e^x dx:", estimated_value_b, "\n");
cat("Valoarea exactă pentru integrala Z 4 1 e^x dx:", exact_value_b, "\n");
cat("Eroarea absolută pentru integrala Z 4 1 e^x dx:", absolute_error_b, "\n");
cat("Eroarea relativă pentru integrala Z 4 1 e^x dx:", relative_error_b, "%", "\n");

# (d) Estimarea integralei Z +∞ 1 dx / (4x^2 - 1)
f_d <- function(x) {return(1 / (4*x^2 - 1));}
integrala2 <- integrate(f_d, 1, Inf)$value;
eroare_abs2 <- abs(integrala2 - log(3)/4);
eroare_rel2 <- eroare_abs2 / abs(log(3)/4);
cat("val reala:",integrate(f_d, 1, Inf)$value);
cat("err abs:",eroare_abs2);
cat("err rel:",eroare_rel2);
#sau varianta 2, aproximand infinitul...
MCimprovedintegration <- function(a, N) {
  sum <- 0
  for (i in 1:N) {
    u <- runif(1, a, a + 10^10)  # Approximating infinity with a large number
    sum <- sum + 1 / (4 * u^2 - 1)
  }
  return(sum/N)
}

exact_values <- c(log(3/4))
estimated_values <- MCimprovedintegration(1, 50000)

# Calculating absolute and relative errors
absolute_errors <- abs(estimated_values - exact_values)
relative_errors <- absolute_errors / exact_values

# Displaying the results
print("Exact values of the integrals:")
print(exact_values)
print("Estimated values of the integrals:")
print(estimated_values)
print("Corresponding absolute errors:")
print(absolute_errors)
print("Corresponding relative errors:")
print(relative_errors)


#2.2
integrand = function(u) {
  return(exp(-2 * u^2))
}
N = 50000
lambda = 3

random_values = rexp(N, rate = lambda)

integrand_values = integrand(random_values)
integral_estimate = (1/N) * sum(integrand_values / dexp(random_values, rate = lambda))

print(paste("Valoarea estimată a integralei:", integral_estimate))

#3.1.
Nrdays_media = function()
{
  nrdays = 1
  lasterrors = c(9, 15, 13)
  nrerrors = sum(lasterrors) # suma celor 3 zile
  while (nrerrors > 0)
  {
    lambda = median(lasterrors) # media celor 3 zile
    nrerrors = rpois(1, lambda) 
    lasterrors = c(nrerrors, lasterrors[1:2]) 
    nrdays = nrdays + 1
  }
  return(nrdays)
}
Nrdays_media()

MCnrdays_media = function(N)
{
  s = 0
  for (i in 1:N)
    s = s + Nrdays_media()
  return(s/N)
}
MCnrdays_media(10000)


#3.2
generate_service_time <- function(lambda) {
  return(rexp(1, rate = lambda))
}

MC_mean_service_time <- function(N) {
  total_service_time <- 0
  for (i in 1:N) {
    if (runif(1) < 0.75) {
      lambda <- 4
    } else {
      lambda <- 12
    }
    service_time <- generate_service_time(lambda)
    total_service_time <- total_service_time + service_time
  }
  return(total_service_time / N)
}

N <- 10000
mean_service_time <- MC_mean_service_time(N)

cat("Media timpului de servire:", mean_service_time, "ore")

#4.1
#a)Estimati probabilitatea de a mai avea inca erori dupa 21 de zile de teste folosind 500 de
#simulari MC. (In primele trei zile numarul de erori gasite este 28, 22 ¸si 18, respectiv.).

Nrdays=function()
{
  nrdays=2 
  lasterrors=c(18,22,28)
  nrerrors=18
  while(nrerrors>0)
  {
    lambda=min(lasterrors)
    nrerrors=rpois(1,lambda)
    lasterrors=c(nrerrors,lasterrors[1:2])
    nrdays=nrdays+1
  }
  return(nrdays) # nr de zile pana cand nu mai are erori
}
Nrdays()

MCnrdays21=function(N)
{
  s=0
  for(i in 1:N)
  {
    if(Nrdays()>21) # ma int daca dupa 21 de zile mai avem erori
      s=s+1 # mai avem erori
  } 
  return(s/N) # probabilitatea
}
MCnrdays21(10000)

alfa = 1 - 0.95
z = qnorm(alfa/2) #T.CENTRALA
epsilon = 0.01 #eroare
#Metoda 1
p = 0.246 #prob
Nmin = p*(1- p)*(z/epsilon)^2 #nr min de simulari
Nmin
MCnrdays21(Nmin + 1)
#Metoda 2
Nmin = (1/4)*(z/epsilon)^2
Nmin
MCnrdays(Nmin + 1)

#4.2
MC_prob_all_infected <- function(N) {
  count_all_infected <- 0
  for (i in 1:N) {
    nr_infected <- 1
    nr_days <- 2
    last_errors <- c(18, 22, 28)
    nr_errors <- 18
    while (nr_errors > 0) {
      lambda <- min(last_errors)
      nr_errors <- rpois(1, lambda)
      last_errors <- c(nr_errors, last_errors[1:2])
      nr_infected <- nr_infected + nr_errors
      nr_days <- nr_days + 1
    }
    if (nr_infected == 40) {
      count_all_infected <- count_all_infected + 1
    }
  }
  return(count_all_infected / N)
}

MC_prob_at_least_15_infected <- function(N) {
  count_at_least_15_infected <- 0
  for (i in 1:N) {
    nr_infected <- 1
    nr_days <- 2
    last_errors <- c(18, 22, 28)
    nr_errors <- 18
    while (nr_errors > 0) {
      lambda <- min(last_errors)
      nr_errors <- rpois(1, lambda)
      last_errors <- c(nr_errors, last_errors[1:2])
      nr_infected <- nr_infected + nr_errors
      nr_days <- nr_days + 1
    }
    if (nr_infected >= 15) {
      count_at_least_15_infected <- count_at_least_15_infected + 1
    }
  }
  return(count_at_least_15_infected / N)
}

MC_prob_at_least_15_infected_error <- function(target_error, confidence) {
  N <- 10000
  p_hat <- MC_prob_at_least_15_infected(N)
  error <- 1
  while (error > target_error) {
    N <- N * 2
    p_hat_prev <- p_hat
    p_hat <- MC_prob_at_least_15_infected(N)
    error <- qnorm(1 - (1 - confidence) / 2) * sqrt(p_hat_prev * (1 - p_hat_prev) / N)
  }
  return(list(probability = p_hat, error = error))
}
# (a) Estimarea probabilității ca într-o anumită zi toate computerele să fie infectate
prob_all_infected <- MC_prob_all_infected(10000)

cat("Probabilitatea ca într-o anumită zi toate computerele să fie infectate:", prob_all_infected, "\n")

# (b) Estimarea probabilității ca într-o anumită zi cel puțin 15 computere să fie infectate
prob_at_least_15_infected <- MC_prob_at_least_15_infected(10000)

cat("Probabilitatea ca într-o anumită zi cel puțin 15 computere să fie infectate:", prob_at_least_15_infected, "\n")

# (c) Estimarea probabilității ca într-o anumită zi cel puțin 15 computere să fie infectate cu o eroare de ±0.01 cu o probabilitate
target_error <- 0.01
confidence <- 0.95

result <- MC_prob_at_least_15_infected_error(target_error, confidence)

cat("Probabilitatea ca într-o anumită zi cel puțin 15 computere să fie infectate:", result$probability, "\n")
cat("Eroarea estimării:", result$error, "\n")

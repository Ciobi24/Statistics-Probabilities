#C1.
MC_volume_fn <- function(N, a) {
  N_C <- 0;
  sqrt_a<-sqrt(a);
  for (i in 1:N) {
    x1 <- runif(1, -sqrt_a, sqrt_a);
    x2 <- runif(1, -sqrt_a, sqrt_a);
    x3 <- x1*x1 + x2*x2;
    if(x3 <= a)
      N_C <- N_C + (a - x3);
  }
  return (4*a * N_C / N)
}
apel_MC<- function(N, a) 
{
  pi = 3.1415926;
  MC_volume <- MC_volume_fn(N, a);
  exact_area = pi*a*a/2
  abs_err <- abs(MC_volume - exact_area)
  relative_error <- abs_err/exact_area
  cat("\nestimated area is", MC_volume)
  cat("\nabsolute error is", abs_err)
  cat("\nrelative error is", relative_error)
}
apel_MC(10000, 2);
apel_MC(10000, 4);
apel_MC(10000, 10);
apel_MC(20000, 2);
apel_MC(20000, 4);
apel_MC(20000, 10);
apel_MC(50000, 2);
apel_MC(50000, 4);
apel_MC(50000, 10);

#C2
MC_area_fn <- function(N)
{
  N_C <- 0;
  for (i in 1:N) {
    x = runif(1, 0, 4);
    if(x<=3)
    {
      y <- (x+6)/3;
    }
    else y <- 3*(4-x);
    N_C <- N_C + y;
    
  }
  return (4 * N_C/N)
}
MC_area <- MC_area_fn(30000);
exact_area = 9; # (3+2)*3/2 + 3*1/2
abs_err <- abs(MC_area - exact_area)
relative_error <- abs_err/exact_area
cat("\nestimated area is", MC_area)
cat("\nabsolute error is", abs_err)
cat("\nrelative error is", relative_error)

#C3
#(a)
MC_area_fn <- function(N, a, b) {
  N_C <- 0;
  for (i in 1:N) {
    x <- runif(1, a, b);
    N_C <- N_C +( (x + 1) /sqrt(4 - x*x) );
  }
  return ((b - a) * N_C / N)
}
MC_integral <- MC_area_fn(10000, -1, 1)
exact_area <- integrate(function(x) ((x + 1) /sqrt(4 - x*x)), -1, 1)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)

#(b)
integrala <- function(N, a) {
  N_C <- 0;
  for (i in 1:N) {
    x <- runif(1, 0, a);
    N_C <- N_C + 1/(4+x*x) 
  }
  return (a * N_C / N)
}

MC_integral <- integrala(100000, 1000);
exact_area <- integrate(function(x)  1/(4+x*x), -Inf, 0)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)

#(c)

integrala <- function(N, a) {
  N_C <- 0;
  for (i in 1:N) {
    x <- -runif(1, 0, a);
    N_C <- N_C + x*exp(x);
  }
  return (a * N_C / N)
}
MC_integral <- integrala(100000, 1000);
exact_area <- integrate(function(x)  x*exp(x), -Inf, 0)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)


#C4
#a)
# Setam valorile parametrilor
m <- 100000  # Numarul maxim de conturi false
n <- 500     # Numarul de conturi noi adaugate in fiecare zi (parametrul pentru distributia binomiala)
p <- 0.5     # Probabilitatea de succes pentru distributia binomiala
q <- 0.1     # Probabilitatea de dezactivare a unui cont fals

num_sims <- 1000  # Numarul de simulari Monte Carlo

# Functie pentru simularea unei singure simulari Monte Carlo
simulate_one_trial <- function() {
  num_days <- 0
  num_false_accounts <- m
  
  max_days <- 365  # Pragul maxim de zile (1 an)
  
  while (num_false_accounts > 0 && num_days <= max_days) {
    num_new_accounts <- rbinom(1, n, p)
    num_false_accounts <- num_false_accounts + num_new_accounts
    
    # Dezactivam conturile false cu probabilitate q
    num_deactivated_accounts <- rbinom(1, num_false_accounts, q)
    num_false_accounts <- num_false_accounts - num_deactivated_accounts
    
    num_days <- num_days + 1
  }
  
  if (num_days > max_days) {
    return("Infinit")
  } else {
    return(num_days)
  }
}

# Simulam mai multe simulari Monte Carlo si inregistram numarul de zile necesare in fiecare simulare
sim_results <- replicate(num_sims, simulate_one_trial())

# Calculam media numarului de zile necesare, excluzand valorile "Infinit"
average_days <- mean(sim_results[sim_results != "Infinit"])

# Afisam rezultatul
if (is.finite(average_days)) {
  cat("Numarul mediu de zile necesare pana cand nu vor mai exista conturi false in retea:", average_days)
} else {
  cat("Reteaua nu va fi niciodata libera de conturi false.")
}
 #b)
m <- 1000  # Numarul maxim de conturi false
n <- 500     # Numarul de conturi noi adaugate in fiecare zi (parametrul pentru distributia binomiala)
p <- 0.5     # Probabilitatea de succes pentru distributia binomiala
days <- 40    # Numarul de zile

# Calculam probabilitatea ca dupa 40 de zile sa avem cel mult 50000 de conturi false
prob_at_most_50000 <- pbinom(50000, m, p^days)

# Afisam rezultatul
cat("Probabilitatea ca dupa 40 de zile sa existe cel mult 50000 de conturi false:", prob_at_most_50000)
  #c)
m <- 10000  # Numarul maxim de conturi false
n <- 500     # Numarul de conturi noi adaugate in fiecare zi (parametrul pentru distributia binomiala)
p <- 0.5     # Probabilitatea de succes pentru distributia binomiala
days <- 40    # Numarul de zile

target_prob <- 0.99  # Probabilitatea tinta
target_error <- 0.01  # Eroarea tinta

num_sims <- 100000  # Numarul de simulari Monte Carlo

# Functie pentru simularea unei singure simulari Monte Carlo si calcularea probabilitatii estimate
simulate_one_trial <- function() {
  num_false_accounts <- m
  for (i in 1:days) {
    num_new_accounts <- rbinom(1, n, p)
    num_false_accounts <- num_false_accounts + num_new_accounts
    num_deactivated_accounts <- rbinom(1, num_false_accounts, q)
    num_false_accounts <- num_false_accounts - num_deactivated_accounts
  }
  
  if (num_false_accounts <= 50000) {
    return(1)
  } else {
    return(0)
  }
}

# Simulam mai multe simulari Monte Carlo si inregistram numarul de simulari in care probabilitatea estimata se incadreaza in intervalul dorit
num_success <- sum(replicate(num_sims, simulate_one_trial()))

# Estimam probabilitatea
estimated_prob <- num_success / num_sims

# Calculam intervalul de incredere
lower_bound <- estimated_prob - target_error
upper_bound <- estimated_prob + target_error

# Verificam daca intervalul de incredere indeplineste cerinta de probabilitate
if (lower_bound >= target_prob || upper_bound <= target_prob) {
  cat("Numarul de simulari nu este suficient pentru a atinge probabilitatea tinta cu eroarea dorita.")
} else {
  cat("Probabilitatea estimata:", estimated_prob, "\n")
  cat("Intervalul de incredere:", lower_bound, "-", upper_bound)
}

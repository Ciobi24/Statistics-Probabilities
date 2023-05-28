
#A1
#a)
grafic=function(k,n,p,l){
  x = k:n
  y_poisson = dpois(l,x)
  y_geometric = dgeom(x, p)
  y_binomial = dbinom(x, n, p)
  
  plot(x,                                           #x-axis values
       y_poisson,                                      #y-axis values
       type = "l",                                  #lines
       main = "Mass Probability Functions",         #title
       col = "blue",                                #color
       ylim = c(min(y_poisson, y_geometric, y_binomial), max(y_poisson, y_geometric, y_binomial)),   #lower and upper limit for the y axis
       xlab = "k:n",                                #x-axis title
       ylab = "Probability")                        #y-axis title
  lines(x, y_geometric, type = "l", col = "red")
  lines(x, y_binomial, type = "l",  col = "green")
}
grafic(3, 10, 0.4, 1)

#b)
#P(X == a) = (1 - p) ^ (a - 1) * p
#P(X = impar) = p / (1 - (1 - p)^2)
geometric = function(p) {
  cat("P(x = odd) = ", p / (1-(1 - p)^2), "\n")
  cat("P(x >= 4) = ", pgeom(4, p, lower.tail = FALSE) + (1 - p) ^ 3 * p, "\n")
  cat("P(x <= 20) = ", pgeom(20, p), "\n")
}
geometric(0.5)

#c)
#P(X == a) = lambda ^ a * e ^ (-lambda) / a!
poisson = function(lambda) {
  k = 0
  limit = 10 ^ (-7)
  rez = exp(-lambda)
  while (ppois(k, lambda, lower.tail = FALSE) + rez < limit) {
    k = k + 1;
    rez = rez * lambda / k
  }
  cat("The least value of k0 such that P(Y >= k0) < 10 ^ (-7) is:", k, "\n")
}

#A2
#a)
statistic <- function(file_name) {
  data=read.csv(file_name)
  probs=data[["P"]]
  stat=data[["S"]]
  
  # Calcularea medianei
  print(median_value_p <- median(probs))
  print(median_value_s <- median(stat))
  # Calcularea mediei
  print(mean_value_p <- mean(probs))
  print(mean_value_s <- mean(stat))
  # Calcularea deviatiei standard
  print(sd_value_p <- sd(probs))
  print(sd_value_s <- sd(stat))
  # Calcularea cvartilelor
  print(quantile_p <- quantile(probs))
  print(quantile_s <- quantile(stat))
  
}
file_name <- "note.csv" 
statistic(file_name)
#b)
remove_outliers <- function(file_name, sample_name) {
  data = read.csv(file_name)
  sample = data[[sample_name]]
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  trimmed_sample = sample[sample >= lower_bound & sample <= upper_bound]
  return (trimmed_sample)
}
remove_outliers("note.csv", "P")
remove_outliers("note.csv", "S")

#c)
plot_distribution <- function(file_name) {
  data <- read.csv(file_name)
  
  # Eliminarea valorilor aberante din esantioane
  trimmed_p <- remove_outliers(file_name, "P")
  trimmed_s <- remove_outliers(file_name, "S")
  
  # Definirea intervalelor
  intervals <- seq(1, 10, by = 1)
  
  # Calcularea frecventelor in intervale
  freq_p <- table(cut(trimmed_p, breaks = intervals, include.lowest = TRUE))
  freq_s <- table(cut(trimmed_s, breaks = intervals, include.lowest = TRUE))
  
  # Reprezentarea grafica a distributiilor
  barplot(freq_p, main = "Distributia frecventelor - Esantion P", xlab = "Intervale", ylab = "Frecventa")
  barplot(freq_s, main = "Distributia frecventelor - Esantion S", xlab = "Intervale", ylab = "Frecventa")
}
file_name <- "note.csv" 
plot_distribution(file_name)

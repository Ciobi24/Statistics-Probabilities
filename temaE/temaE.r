#I
zconfidence_interval = function(alpha, n, sample_mean, sigma) {
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
zconfidence_interval(1 - 0.9, 10, 138, 11)
zconfidence_interval(1 - 0.95, 10, 138, 11)
zconfidence_interval(1 - 0.99, 10, 138, 11)

#II
zconfidence_interval(1 - 0.95, 256, 18, sqrt(1.44))

#III
schimbare = function(lvl, n, succes, p0, tip_ip) {
  p_prim = succes / n
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  cat(z_score, "\n")
  if (tip_ip == "r") {
    critical_z = qnorm(1 - lvl, 0, 1)
    cat(critical_z, "\n")
    if (z_score > critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  } else if (tip_ip == "l") {
    critical_z = qnorm(1 - lvl, 0, 1)
    cat(critical_z, "\n")
    if (z_score < critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  } else {
    critical_z = qnorm(1 - lvl / 2, 0, 1)
    cat(critical_z, "\n")
    if (abs(z_score) > critical_z) {
      cat("we reject H0\n")
    } else {
      cat("we don't have enough evidence to reject H0\n")
    }
  }
}
schimbare(0.01, 153, 17, 0.12, "r")
schimbare(0.05, 153, 17, 0.12, "r")

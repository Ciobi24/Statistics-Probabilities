#D1
MC_find_M_element = function(x, k) {
  i = 0
  while (i < k) {
    number = sample(x, 1)
    count = 0
    j = 1
    while (j <= length(x)) {
      if (x[j] == number) {
        count = count + 1
      }
      j = j + 1
    }
    if (count >= length(x) / 2 + 1) {
      return (number)
    }
    i = i + 1
  }
  return ("x nu are niciun M-element")
}
x = sample(0:1, 100, replace = TRUE)
# 1/2^k<=1/10^7
# 2^k>=10^7 
# k=24 de ex:
MC_find_M_element(x, 24)

#D2
# trebuie ca vectorul sa fie sortat ca sa fie corect
element_ith = function(i, A) {
  z = sample(A, 1)
  Alt = A[A < z]
  Agt = A[A > z]
  if (length(Alt) > i) {
    return (element_ith(i, Alt))
  }
  else {
    if (length(A) > i + length(Agt)) {
      return (z);
    }
    else {
      return (element_ith(i - length(A) + length(Agt), Agt))
    }
  }
}

#D3
MC_median = function(S, a) {
  if (a < 0) {
    a = -a
  }
  m = floor(a * log(length(S)))
  SS = sample(S, m)
  sorted_SS = sort(SS)
  median_index = ceiling(m / 2)
  return (sorted_SS[median_index])
}
# 1-2/n^2>=1-10^(-7)
# 2/n^2<=10^(-7)
# n^2/2>=10^7
# n^2>=2*10^7
# n>=sqrt(2*10^7)
# n>=4472.13595
x = runif(4473, 0, 100)
median(x)
MC_median(x, 532)

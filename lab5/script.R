tree_eval = function(i, leaves) {
  a = runif(1, 0, 1); len = length(leaves);
  if(log(i,2) >= log(len,2) - 1) { # copiii nodului i sunt frunze
    if(a <= 0.5) {
      if(leaves[2*i - len + 1] == 0)
        return(leaves[2*i +1 -len + 1])
               return(0);}
    else {
      if(leaves[2*i + 1 -len + 1] == 0)
        return(leaves[2*i -len + 1])
               return(0);}
  }
  if((floor(log(i,2))%% 2 == 0)){ # nodul i este de tip MIN
    if(a <= 0.5) {
      if(tree_eval (2*i, leaves) == 1)
        return(tree_eval (2*i + 1, leaves));
      return(1);}
    else {
      if(tree_eval (2*i +1, leaves) == 1)
        return(tree_eval (2*i, leaves));
      return(1);
    }
  }
  if((floor(log(i,2))%% 2 == 1)){ # nodul i este de tip MAX
    if(a <= 0.5) {
      if(tree_eval (2*i, leaves) == 0)
        return(tree_eval (2*i + 1, leaves));
      return(1);}
      else {
        if(tree_eval (2*i +1, leaves) == 0)
          return(tree_eval (2*i, leaves));
        return(1);
      }
    }
}   
leaves=c(0,1,0,1,1,1,1,0,0,1,0,1,1,0,0,0)
tree_eval(1,leaves)

#SIMULARE VARIABILA ALEATOARE
sim_variabila <- function(val, prob) {
  if (length(val) != length(prob)) {
    stop("Input gresit")
  }
  
  # nr aleatoriu între 0 și 1
  p <- runif(1)
  
  # Iniț. suma prob cu 0
  suma_prob <- 0
  
  # Parcurgem val și prob
  for (i in 1:length(val)) {
    suma_prob <- suma_prob + prob[i]
    
   # Dacă suma probabilităților depășește numărul generat aleatoriu, returnăm valoarea curentă
    if (p <= suma_prob) {
      return(val[i])
    }
  }
  stop("Probabilitățile date nu sunt valide.")
}

# Ex
val <- c('1', '2', '3')
prob <- c(0.2, 0.3, 0.5)

exemplu <- sim_variabila(val, prob)
print(exemplu)
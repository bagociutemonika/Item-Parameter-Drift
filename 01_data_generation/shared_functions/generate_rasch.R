# Rasch data generator

generate_rasch <- function(theta, b_vec){
  
  N <- length(theta)
  J <- length(b_vec)
  
  resp <- matrix(NA, nrow=N, ncol=J)
  
  for(i in 1:J){
    p <- 1/(1+exp(-(theta - b_vec[i])))
    resp[,i] <- rbinom(N, 1, p)
  }
  
  colnames(resp) <- names(b_vec)
  resp
}



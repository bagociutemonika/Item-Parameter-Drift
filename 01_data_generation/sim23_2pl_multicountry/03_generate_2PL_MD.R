
# Simulation 2.3 — Multidimensional 2PL Generator


generate_2PL_MD <- function(theta_vec,
                            a_vec,
                            b_vec,
                            dims){
  
  J <- length(a_vec)
  
  resp <- matrix(NA, 1, J)
  
  for(i in 1:J){
    
    theta <- theta_vec[dims[i]]
    
    p <- 1 / (
      1 + exp(
        -a_vec[i] * (theta - b_vec[i])
      )
    )
    
    resp[,i] <- rbinom(1,1,p)
  }
  
  colnames(resp) <- names(a_vec)
  
  resp
}
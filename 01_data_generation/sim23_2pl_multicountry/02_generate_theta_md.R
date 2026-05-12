
# Simulation 2.3 — Ability Generators



# Unidimensional country generator


generate_theta <- function(country, cycle, N){
  
  mu <- base_mu[country] +
    trend_noisy[country, as.character(cycle)]
  
  sd <- sigma_theta[country]
  
  rnorm(
    N,
    mean = mu,
    sd   = sd
  )
}


# Multidimensional generator


generate_theta_md <- function(country, cycle, N){
  
  mu_main <- base_mu[country] +
    trend_noisy[country, as.character(cycle)]
  
  sd_country <- sigma_theta[country]
  
  theta_main <- rnorm(
    N,
    mu_main,
    sd_country
  )
  
  theta_resid <- MASS::mvrnorm(
    N,
    mu = c(0,0,0),
    Sigma = Sigma
  )
  
  theta_matrix <- cbind(
    theta_main + theta_resid[,1],
    theta_main + theta_resid[,2],
    theta_main + theta_resid[,3]
  )
  
  theta_matrix
}
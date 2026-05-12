
J_total  <- 75
J_anchor <- 15
J_non    <- J_total - J_anchor

all_items    <- paste0("I",1:J_total)
anchor_items <- paste0("I",1:J_anchor)
non_anchor   <- paste0("I",(J_anchor+1):J_total)

cycles <- c(2015, 2018)
countries <- c("JPN","SPN","TUR")

N_per_group <- 2000



#Country generator



generate_theta <- function(country, cycle, N){
  
  mu <- base_mu[country] +
    trend_noisy[country, as.character(cycle)]
  
  sd <- sigma_theta[country]
  
  rnorm(N, mean = mu, sd = sd)
  
}


#Booklet design 

block1 <- non_anchor[1:15]
block2 <- non_anchor[16:30]
block3 <- non_anchor[31:45]
block4 <- non_anchor[46:60]

booklets <- list(
  B1 = c(anchor_items, block1),
  B2 = c(anchor_items, block2),
  B3 = c(anchor_items, block3),
  B4 = c(anchor_items, block4)
)



#Assign Items to Dimensions


dim_assignment <- rep(1:3, length.out = J_total)
names(dim_assignment) <- all_items


#True Item Parameters


a_true <- rlnorm(J_total,0,.2)
names(a_true) <- all_items

b_true <- rnorm(J_total,0,1)
names(b_true) <- all_items



base_mu <- c(
  JPN = 0.42,
  SPN = -0.04,
  TUR = -0.70
)

trend <- rbind(
  JPN = c(`2015`=0.00, `2018`=-0.05),
  SPN = c(`2015`=0.00, `2018`=-0.05),
  TUR = c(`2015`=0.00, `2018`=0.33)
)

sigma_theta <- runif(length(countries),.9,1.1)
names(sigma_theta) <- countries

trend_noise_sd <- .03
trend_noisy <- trend +
  matrix(
    rnorm(length(trend),0,trend_noise_sd),
    nrow=nrow(trend),
    dimnames=dimnames(trend)
  )



# Correlated latent dimensions


Sigma <- matrix(
  c(
    1,.8,.8,
    .8,1,.8,
    .8,.8,1
  ),
  3,3
)

# Experimental conditions


drift_magnitudes <- c(0.2,0.5,1.0)

drift_proportions <- c(0.1,0.2,0.3)

drift_types <- c("a","b","ab")

# Each function takes in the iteration number (iter) which affects the seed number 
# and subsequently random number generation for the multivariate normal processes

# For each simulation, there are two inputs:

# iter = the iteration number - usually corresponds to the loop/repetition number
# return = by default is "y", the entire multivariate simulation. However, if set
#          to "sigma", will return a list of the original covariance matrix values

##### Simulation 2 #####
sim2 = function(iter, return = "y"){
  # Time points per segment
  t       = 100
  # Nodes per cluster
  s.p     = 200
  # Clusters
  K       = 2
  n.tot   = K * s.p
  # Within cluster correlation
  rho.wi  = 0.75
  # Between cluster correlation
  rho.bw  = 0.2
  # Covariance of first segment
  Sigma1 	      = group.corr(small.n = s.p, rho = rho.wi, p = n.tot)
  ind 	      = which(Sigma1 == 0)
  Sigma1[ind]   = rho.bw
  
  # Set up the simulation
  set.seed(iter*7)
  reorder = sample(c(1:n.tot), n.tot)
  Sigma2  = Sigma1[reorder, reorder]
  set.seed(iter*3)
  Y1      = mvrnorm(n = t, mu = rep(0, n.tot), Sigma = Sigma1)
  set.seed(iter*4)
  Y2 		  = mvrnorm(n = t, mu = rep(0, n.tot), Sigma = Sigma2)
  y 		  = rbind(Y1, Y2)
  
  # Return the output
  if(return == "y"){
    # Return the simulation
    return(y)
  } else if(return == "sigma"){
    # Return original covariance matrices as list
    return(list(Sigma1,
                Sigma2))
  }
}
##### Simulation 2 #####
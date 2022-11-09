
  model{
    # priors
    b0 ~ dnorm(0, (1/2.7)) # dnorm(mu = mean, tau= precision )
    b1 ~ dnorm(0, (1/2.7))
    b2 ~ dnorm(0, (1/2.7))
    b3 ~ dnorm(0, (1/2.7))
    p ~ dunif(0, 1)
    # likelihood
    for(i in 1:N){
      # deterministic model
      psi[i] <- ilogit(
         b0
         + b1*elevation[i]
         + b2*(elevation[i]^2)
         + b3*forestCover[i]
      )
      # Deterministic model of z (the true probability of occupancy)
        z[i] ~ dbern(psi[i])
      # likelihood
        # Stochastic model of y (the observed detections)
        y[i] ~ dbin(z[i] * p, n[i])
        y_sim[i]  ~ dbin(z[i] * p, n[i])
    }
    # Derived quantities
      #posterior predictive checks
        mean_y = mean(y)
        sd_y = sd(y)
        mean_y_sim = mean(y_sim)
        sd_y_sim = sd(y_sim)
        p_val_mean = step(mean_y_sim - mean_y)
        p_val_sd = step(sd_y_sim - sd_y)
      # sum of squares 
        for(j in 1:N) {
          sq[j] <- (y[j]-psi[j])^2
          sq_sim[j] <- (y_sim[j]-psi[j])^2
        }
        fit_y <- sum(sq)
        fit_y_sim <- sum(sq_sim)
        p_val_fit = step(fit_y_sim - fit_y)
  }
  

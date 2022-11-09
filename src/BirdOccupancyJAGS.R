
  model{
    # priors
    b0 ~ dnorm(0, (1/2.7)) # dnorm(mu = mean, tau= precision )
    b1 ~ dnorm(0, (1/2.7))
    b2 ~ dnorm(0, (1/2.7))
    b3 ~ dnorm(0, (1/2.7))
    p ~ dunif(0, 1)
    # likelihood
    for(i in 1:N){
      # Deterministic model of z (the true probability of occupancy)
      psi[i] <- ilogit(
         b0
         + b1*elevation[i]
         + b2*(elevation[i]^2)
         + b3*forestCover[i]
      )
      # likelihood
        z[i] ~ dbern(psi[i])
      # likelihood
        # Stochastic model of y (the observed detections)
        y[i] ~ dbin(z[i] * p, n[i])
        y_sim[i]  ~ dbin(z[i] * p, n[i])
      # sum of squares 
        sq[i] <- (y[i]-psi[i])^2
        sq_sim[i] <- (y_sim[i]-psi[i])^2
    }
    # Derived quantities
      #posterior predictive checks
        # test statistics y
        mean_y <- mean(y)
        sd_y <- sd(y)
        fit_y <- sum(sq)
        # test statistics y_sim
        mean_y_sim <- mean(y_sim)
        sd_y_sim <- sd(y_sim)
        fit_y_sim <- sum(sq_sim)
        # p-values
        p_val_mean <- step(mean_y_sim - mean_y)
        p_val_sd <- step(sd_y_sim - sd_y)
        p_val_fit <- step(fit_y_sim - fit_y)
      # optimum elevation of willow tit habitat at the mean forest cover
        # ... where optimum elevation is defined as the elevation 
        # ... where probability of occupancy is maximum.
        # f(x) = ax^2 + bx + c
        # x-value of the vertex of the parabola = -b/2a 
        optimum_elev_z = -b1/(2*b2)
        optimum_elev = (optimum_elev_z * sd_elevation) + mean_elevation
        # The predicted probability of occupancy
        for(j in 1:length(elevation_pred_z)){
          p_est[j] <- ilogit(
               b0
               + b1*elevation_pred_z[j]
               + b2*(elevation_pred_z[j]^2)
               # + b3*mean_forestCover
            )
        }
  }
  

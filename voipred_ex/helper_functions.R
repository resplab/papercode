dca <- function(val_data, zs=(0:99)/100, weights=NULL)
{
  n <- dim(val_data)[1]
  
  NB_model <- NB_all <- rep(0, length(zs))
  
  if(is.null(weights)) weights <- rep(1,n)
    
  for(j in 1:length(zs))
  {
    NB_model[j] <- sum(weights*(val_data$pi>zs[j])*(val_data$Y-(1-val_data$Y)*zs[j]/(1-zs[j])))/n
    NB_all[j] <- sum(weights*(val_data$Y-(1-val_data$Y)*zs[j]/(1-zs[j])))/n
  }
  
  return(data.frame(z=zs, NB_model=NB_model, NB_all=NB_all))
}

bootstrap <- function (n, Bayesian=F, weights=NULL)
{
  if(Bayesian)
  {
    u <- c(0,sort(runif(n-1)),1)
    u <- (u[-1] - u[-length(u)])*n
    if(!is.null(weights)) u <- u*weights*n/sum(u*weights)
  }
  else
  {
    if(is.null(weights)) weights <-rep(1/n,n)
    u <- rmultinom(1,n,weights)
  }
  as.vector(u)
}

evpi_val <- function (Y, pi, method = c("bootstrap", "bayesian_bootstrap", 
                                        "asymptotic"), n_sim = 1000, zs = (0:99)/100, weights = NULL) 
{
  n <- length(Y)
  if (method == "asymptotic") {
    if (is.null(weights)) 
      weights <- rep(1, n)
    ENB_perfect <- ENB_current <- rep(0, length(zs))
    for (j in 1:length(zs)) {
      NB_model <- sum(weights * (pi > zs[j]) * (Y - (1 - 
                                                       Y) * zs[j]/(1 - zs[j])))/n
      NB_all <- sum(weights * (Y - (1 - Y) * zs[j]/(1 - 
                                                      zs[j])))/n
      parms <- calc_NB_moments(Y, pi, zs[j], weights)
      if (is.na(parms[5])) {
        ENB_perfect[j] <- ENB_current[j] <- max(0, NB_model, 
                                                NB_all)
      }
      else {
        if (parms[5] > 0.999999) 
          parms[5] <- 0.999999
        if (parms[5] < -0.999999) 
          parms[5] <- -0.999999
        tryCatch({
          ENB_perfect[j] <- ENB_perfect[j] <- mu_max_trunc_bvn(parms[1],parms[2],parms[3],parms[4],parms[5])
        }, error = function(cond) {
          return(NULL)
        })
        ENB_current[j] <- max(0, NB_model, NB_all)
      }
    }
    return(data.frame(z = zs, ENB_perfect = ENB_perfect, 
                      ENB_current = ENB_current, EVPIv = ENB_perfect - 
                        ENB_current))
  }
  NB_model <- NB_all <- matrix(0, n_sim, ncol = length(zs))
  if (method == "bootstrap" || method == "bayesian_bootstrap") {
    Bayesian_bootstrap <- method == "bayesian_bootstrap"
    for (i in 1:n_sim) {
      w_x <- bootstrap(n, Bayesian_bootstrap, weights = weights)
      for (j in 1:length(zs)) {
        NB_model[i, j] <- sum(w_x * (pi > zs[j]) * (Y - 
                                                      (1 - Y) * zs[j]/(1 - zs[j])))/n
        NB_all[i, j] <- sum(w_x * (Y - (1 - Y) * zs[j]/(1 - 
                                                          zs[j])))/n
      }
    }
  }
  else {
    stop("Method ", method, " is not recognized.")
  }
  ENB_model <- ENB_all <- ENB_perfect <- ENB_current <- EVPIv <- p_useful <- rep(NA, 
                                                                                 length(zs))
  for (i in 1:length(zs)) {
    ENB_model[i] <- mean(NB_model[, i])
    ENB_all[i] <- mean(NB_all[, i])
    ENB_perfect[i] <- mean(pmax(NB_model[, i], NB_all[, i], 
                                0))
    ENB_current[i] <- max(ENB_model[i], ENB_all[i], 0)
    EVPIv[i] <- ENB_perfect[i] - ENB_current[i]
    p_useful[i] <- mean((pmax(NB_model[, i], NB_all[, i], 
                              0) - NB_model[, i]) == 0)
  }
  data.frame(z = zs, ENB_model = ENB_model, ENB_all = ENB_all, 
             ENB_current = ENB_current, ENB_perfect = ENB_perfect, 
             EVPIv = EVPIv, p_useful = p_useful)
}

